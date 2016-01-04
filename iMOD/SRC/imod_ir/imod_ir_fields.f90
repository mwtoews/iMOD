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
MODULE MOD_IR_FIELDS

USE WINTERACTER
USE RESOURCE
USE MOD_UTL, ONLY : ITOS,RTOS,LISTNAME,UTL_IMODFILLMENU,UTL_CAP,UTL_GETUNIT,UTL_MESSAGEHANDLE,IDFPLOT1BITMAP, &
   IDFPLOT2BITMAP,UTL_INSIDEPOLYGON
USE MOD_POLYGON_PAR
USE MOD_POLYGON_DRAW
USE MOD_POLYGON_UTL, ONLY : POLYGON1FIELDS
USE MOD_IR_PAR
USE MOD_IR_UTL, ONLY : IR1GETTREEVIEWID,IR1POL2SHAPE,IR1SHAPE2POL,IR1GETTREEID,IR1FIELDS_STRING,IR1GETTREEIDS,IR1DIRNAME, &
                       IR1FIELDS_GETIPERIRES
USE MOD_IR_CLC, ONLY : IR2GETEXTENSION
USE MOD_IR_SELECTEDCELLS, ONLY : IR_GETSELECTEDCELLS,IR_SELECTEDCELLS
USE MOD_IDF, ONLY : IDFDEALLOCATE,IDFWRITE,IDFREAD,IDFGETVAL,IDFCOPY,IDFGETLOC,IDFIROWICOL,IDFNULLIFY
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_LEGPLOT, ONLY : LEGPLOT_MAIN
USE MOD_PREF_PAR, ONLY : PREFVAL
USE IMOD

INTEGER :: ICUR_IFIELD,ICUR_ITREE !## previous tree-field
INTEGER,PRIVATE :: NLIST
CHARACTER(LEN=256),PRIVATE :: IDFNAME,DIRNAME
CHARACTER(LEN=256),PRIVATE,DIMENSION(:),ALLOCATABLE :: IDFRESLIST
INTEGER(KIND=1),ALLOCATABLE,DIMENSION(:,:),PRIVATE :: IP
TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:),PRIVATE :: IDFDIFF

CONTAINS

 !###======================================================================
 SUBROUTINE IR1FIELDS_MAIN()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,ITAB,ITREE,IFIELD,ITARGET,IMEASURE
 LOGICAL :: LTREE

 !## remove previous polygons non-editable/editable - only if available
 IF(ICUR_ITREE.NE.0)THEN

  !## remove current polygon and if related: ysel
  CALL POLYGON1DRAWSHAPE(1,SHPNO)

  !## read adjustments from previous tab
  SELECT CASE (ICUR_ITREE)
   CASE (1)
    !## copy data - SHAPES -> POL
    CALL IR1SHAPE2POL(ICUR_ITREE,ICUR_IFIELD)
   CASE (2)
    !## copy data - SHAPES -> POL
    CALL IR1SHAPE2POL(ICUR_ITREE,ICUR_IFIELD)
   CASE (3)
    !## copy data - SHAPES -> POL - adjustments to target on tab results
    CALL IR1GETTREEIDS(ICUR_IFIELD,ITARGET,IMEASURE)
    CALL IR1SHAPE2POL(1,ITARGET)
  END SELECT

 ENDIF

!WRITE(*,*) 1,icur_itree,icur_ifield
!pause

 !## update ICUR_ITREE/ICUR_IFIELD - te reflect current itree
 CALL IR1GETTREEVIEWID(ITREE,IFIELD)

 LTREE=ICUR_ITREE.NE.ITREE

 IF(LTREE)THEN
  !## (remove) current polygons as if they were non-editable
  CALL IR1DRAWSHAPES(1)
 ENDIF

 ICUR_ITREE =ITREE
 ICUR_IFIELD=IFIELD

 !## use targets in case of results
 IF(ITREE.EQ.3)THEN
  CALL IR1GETTREEIDS(IFIELD,ITARGET,IMEASURE)
  ICUR_ITREE =1
  ICUR_IFIELD=ITARGET
 ENDIF

 !## remains identical level in tree
 IF(LTREE)THEN
  !## draw current polygons (non-editable)
  CALL IR1DRAWSHAPES(1)
 ENDIF

!WRITE(*,*) 2,icur_itree,icur_ifield
!PAUSE

 ACTID=1
 SELECT CASE (ITREE)!ICUR_ITREE)
  CASE (1)
   ACTID(3)=0
  CASE( 2)
   ACTID(2)=0
  CASE (3)
   ACTID(2)=0
   ACTID(3)=0
 END SELECT

 DO I=1,3

  CALL WINDOWSELECT(IRWIN)
  CALL WMENUSETSTATE(BUTID(I),1,ACTID(I))

  CALL WDIALOGSELECT(ID_DIR_PM)
  CALL WDIALOGFIELDSTATE(BUTID(I),ACTID(I))

  IF(I.EQ.ITREE)THEN!ICUR_ITREE)THEN
   CALL WDIALOGTABSTATE(IDF_TAB,TABID(I),1)
   CALL WDIALOGSETTAB(IDF_TAB,TABID(I))
  ELSE
   CALL WDIALOGTABSTATE(IDF_TAB,TABID(I),0)
  ENDIF
 END DO

!WRITE(*,*) 3,ICUR_ITREE,ICUR_IFIELD

 !## new tab - copy pol to shapes (for interaction purposes)
 SELECT CASE (ITREE)!CUR_ITREE)
  !## tab: targets
  CASE (1)
   !## copy data
   CALL IR1POL2SHAPE(ICUR_ITREE,ICUR_IFIELD)
   CALL IR1FIELDS_TAB1()
   CALL WDIALOGSELECT(ID_DIR_PMTAB1TAB2)
   LPLOTYSEL=.FALSE.
   !## draw current polygon
   CALL POLYGON1DRAWSHAPE(1,SHPNO)
  !## tab: measures
  CASE (2)
   !## copy data
   CALL IR1POL2SHAPE(ICUR_ITREE,ICUR_IFIELD)
   CALL IR1FIELDS_TAB2()
   CALL WDIALOGSELECT(ID_DIR_PMTAB2)
   CALL WDIALOGGETCHECKBOX(IDF_CHECK2,I)
   IF(I.EQ.0)LPLOTYSEL=.FALSE.
   IF(I.EQ.1)LPLOTYSEL=.TRUE.
   CALL IR_GETSELECTEDCELLS()
   CALL WDIALOGSELECT(ID_DIR_PMTAB2TAB2)
   !## draw current polygon
   CALL POLYGON1DRAWSHAPE(1,SHPNO)
  !## tab: results
  CASE (3)
   !## copy data
   CALL IR1POL2SHAPE(ICUR_ITREE,ICUR_IFIELD)!1,ITARGET)
   CALL IR1FIELDS_TAB1()
   LPLOTYSEL=.FALSE.
   CALL WDIALOGSELECT(ID_DIR_PMTAB3)
   CALL WDIALOGGETTAB(IDF_TAB1,ITAB)
   !## draw effects
   IF(ITAB.EQ.ID_DIR_PMTAB3TAB1)THEN
    CALL IR1FIELDS_TAB3TAB1()
   !## draw legend
   ELSEIF(ITAB.EQ.ID_DIR_PMTAB3TAB2)THEN
    CALL IR1FIELDS_TAB3TAB2()
   ENDIF

 END SELECT

 SELECT CASE (ITREE)!ICUR_ITREE)
  CASE (1,2)
   IF(SHPNO.EQ.0)THEN
    CALL WDIALOGCLEARFIELD(IDF_MENU1)
   ELSE
    CALL WDIALOGPUTMENU(IDF_MENU1,SHPNAME,SHPNO,SHPIACT)
   ENDIF
   IF(ITREE.EQ.1)CALL POLYGON1FIELDS(ID_DIR_PMTAB1TAB2)
   IF(ITREE.EQ.2)CALL POLYGON1FIELDS(ID_DIR_PMTAB2TAB2)
 END SELECT

! CALL WDIALOGSELECT(ID_DIR_PM)
! CALL WDIALOGSETFIELD(IDF_TREEVIEW1)
 I=0
 IF(NMEASURE.GT.0.AND. &
    ITREE.NE.3.AND. &
   (NTARGET.GT.1.OR.ITREE.NE.1))I=1
!WRITE(*,*) NTARGET,NMEASURE,ITREE
!WRITE(*,*) I,NMEASURE.GT.0,ITREE.NE.3,(NTARGET.GT.1.OR.ITREE.NE.1)
 CALL WDIALOGSELECT(ID_DIR_PM)
 CALL WDIALOGFIELDSTATE(ID_COPY,I)
 CALL WDIALOGFIELDSTATE(ID_DELETE,I)
 CALL WINDOWSELECT(IRWIN)
 CALL WMENUSETSTATE(ID_COPY,1,I)
 CALL WMENUSETSTATE(ID_DELETE,1,I)

!WRITE(*,*) 3,icur_itree,icur_ifield
!PAUSE

! CALL IR1FIELDS_STRING(CTREE,ITREE,IFIELD)
! CALL WINDOWSELECT(IRWIN)
! DO I=1,ICUR_ITREE
!  IF(I.EQ.1)STRING='Navigator: '//TRIM(CTREE(I))
!  IF(I.NE.1)STRING=TRIM(STRING)//'; '//TRIM(CTREE(I))
! END DO
! CALL WINDOWOUTSTATUSBAR(1,TRIM(STRING))

 END SUBROUTINE IR1FIELDS_MAIN

 !###======================================================================
 SUBROUTINE IR1DRAWSHAPES(ISEQ)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ISEQ
 INTEGER :: N,IPOL,IFIELD1,IFIELD2!,ITREE1,ITREE2,ITARGET,IMEASUREI

 CALL IDFPLOT1BITMAP()

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
   CALL IGRCOLOURN(INVERSECOLOUR(TTREE(IFIELD1)%POL(IPOL)%ICLR))
   N=TTREE(IFIELD1)%POL(IPOL)%NCRD
   CALL IGRPOLYGONCOMPLEX(TTREE(IFIELD1)%POL(IPOL)%X,TTREE(IFIELD1)%POL(IPOL)%Y,N)
  ENDDO
 ENDIF
 !## measure
 IF(IFIELD2.NE.0)THEN
  !## get selected ipol
  DO IPOL=1,MTREE(IFIELD2)%NPOL
   CALL IGRCOLOURN(INVERSECOLOUR(MTREE(IFIELD2)%POL(IPOL)%ICLR))
   N=MTREE(IFIELD2)%POL(IPOL)%NCRD
   CALL IGRPOLYGONCOMPLEX(MTREE(IFIELD2)%POL(IPOL)%X,MTREE(IFIELD2)%POL(IPOL)%Y,N)
  ENDDO
 ENDIF

 CALL IDFPLOT2BITMAP()

 CALL IGRLINEWIDTH(1)
 CALL IGRPLOTMODE(MODECOPY)

 END SUBROUTINE IR1DRAWSHAPES

 !###======================================================================
 SUBROUTINE IR1FIELDS_TAB1()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,K,ITREE,IFIELD

 I=0
 K=0
 IF(SHPNO.GT.0.AND.SUM(SHPIACT(1:SHPNO)).EQ.1)I=1
 IF(SHPNO.LT.MAXSHAPES)K=1

 CALL WDIALOGSELECT(ID_DIR_PMTAB1TAB1)

 !## fields on targets tab
 CALL WDIALOGFIELDSTATE(ID_ADD,I)
 IF(SHPNO.LE.1)CALL WDIALOGFIELDSTATE(ID_COPY,0)
 IF(SHPNO.GT.1)CALL WDIALOGFIELDSTATE(ID_COPY,1)
! CALL WDIALOGFIELDSTATE(ID_COPY,I)
 CALL WDIALOGFIELDSTATE(IDF_STRING1,1)

 !## get level of treeview
 CALL IR1GETTREEVIEWID(ITREE,IFIELD)

 CALL WDIALOGSELECT(ID_DIR_PMTAB1TAB1)
 IF(I.EQ.0)THEN
  IF(SUM(SHPIACT(1:SHPNO)).EQ.0)CALL WDIALOGPUTSTRING(IDF_STRING1,'No polygon selected. You need to create and/or select ONE '//&
  'polygon to be able to assign a target definition to it.')
  IF(SUM(SHPIACT(1:SHPNO)).GT.1)CALL WDIALOGPUTSTRING(IDF_STRING1,'More than one polygon selected. You need to select ONE '//&
  'polygon to be able to assign a target definition to it.')
 ELSE
  !## fill field
  CALL IR1FIELDS_WRITETAB1(IFIELD)
 ENDIF

 !## fields on polygons
 CALL WDIALOGSELECT(ID_DIR_PMTAB1TAB2)
 IF(NBC.LE.0)K=0
 CALL WDIALOGFIELDSTATE(ID_GENCOPY,K)

 END SUBROUTINE IR1FIELDS_TAB1

 !###======================================================================
 SUBROUTINE IR1FIELDS_WRITETAB1(IFIELD)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IFIELD
 INTEGER :: I,INEWT,INEWP,IPOL
 REAL :: LOWER,UPPER
 CHARACTER(LEN=500) :: TXT

 CALL WDIALOGSELECT(ID_DIR_PMTAB1TAB1)

 DO IPOL=1,TTREE(IFIELD)%NPOL
  IF(SHPIACT(IPOL).EQ.1)EXIT
 ENDDO

! TXT='Assigned definition for polygon:'//CHAR(13)//CHAR(10)//TRIM(TTREE(IFIELD)%POL(IPOL)%POLNAME)//CHAR(13)//CHAR(10)

 IF(TTREE(IFIELD)%POL(IPOL)%NDEF.GT.0)THEN
  TXT='Assigned definition for polygon:'//CHAR(13)//CHAR(10)//TRIM(TTREE(IFIELD)%POL(IPOL)%POLNAME)//CHAR(13)//CHAR(10)
  !## fill in measures
  DO I=1,TTREE(IFIELD)%POL(IPOL)%NDEF
   INEWT=TTREE(IFIELD)%POL(IPOL)%DEF(I)%INEWT
   INEWP=TTREE(IFIELD)%POL(IPOL)%DEF(I)%INEWP
   LOWER=TTREE(IFIELD)%POL(IPOL)%DEF(I)%LOWER
   UPPER=TTREE(IFIELD)%POL(IPOL)%DEF(I)%UPPER
   TXT=TRIM(TXT)//CHAR(13)//CHAR(10)//TRIM(PER(INEWP)%NAMEPER)//' '//TRIM(RES(INEWT)%NAMERES)
   TXT=TRIM(TXT)//' ['//TRIM(RTOS(LOWER,'F',2))//' - '//TRIM(RTOS(UPPER,'F',2))//']'
  END DO
!  LOWER=TTREE(IFIELD)%POL(IPOL)%EFFECT
!  TXT=TRIM(TXT)//CHAR(13)//CHAR(10)//CHAR(13)//CHAR(10)//'The water table target(s) should be met for at least '// &
!      TRIM(RTOS(LOWER,'F',2))//'% of polygon area. The flux target(s) should be met by the average flux effect in the polygon area.'
  TXT=TRIM(TXT)//CHAR(13)//CHAR(10)//CHAR(13)//CHAR(10)//'The target(s) should be met by the average effect in the polygon area.'
 ELSE
  TXT='No targets are specified yet!'
 ENDIF

 CALL WDIALOGPUTSTRING(IDF_STRING1,TXT)

 END SUBROUTINE IR1FIELDS_WRITETAB1

 !###======================================================================
 SUBROUTINE IR1FIELDS_TAB2()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,K,ITREE,IFIELD

! WRITE(*,*) 'TAB2',SHPNO,SUM(SHPIACT(1:SHPNO))

 I=0
 J=0
 K=0
 IF(SHPNO.GT.0.AND.SUM(SHPIACT(1:SHPNO)).EQ.1)I=1
 IF(SHPNO.GT.0.AND.SUM(SHPIACT(1:SHPNO)).GT.0)J=1
 IF(SHPNO.LT.MAXSHAPES)K=1

 !## fields on measures - maximal one selected
 CALL WDIALOGSELECT(ID_DIR_PMTAB2TAB1)
 CALL WDIALOGFIELDSTATE(ID_ADD,I)
 IF(SHPNO.LE.1)CALL WDIALOGFIELDSTATE(ID_COPY,0)
 IF(SHPNO.GT.1)CALL WDIALOGFIELDSTATE(ID_COPY,1)
! CALL WDIALOGFIELDSTATE(ID_COPY,I)
 CALL WDIALOGFIELDSTATE(IDF_STRING1,1)

 !## get level of treeview
 CALL IR1GETTREEVIEWID(ITREE,IFIELD)

 CALL WDIALOGSELECT(ID_DIR_PMTAB2TAB1)
 IF(I.EQ.0)THEN
  IF(SUM(SHPIACT(1:SHPNO)).EQ.0)CALL WDIALOGPUTSTRING(IDF_STRING1,'No polygon selected. You need to create and/or select ONE '//&
  'polygon to be able to assign a measure definition to it.')
  IF(SUM(SHPIACT(1:SHPNO)).GT.1)CALL WDIALOGPUTSTRING(IDF_STRING1,'More than one polygon selected. You need to select ONE '//&
  'polygon to be able to assign a measure definition to it.')
 ELSE
  !## fill field
  CALL IR1FIELDS_WRITETAB2(IFIELD)
 ENDIF

 !## fields on polygons
 CALL WDIALOGSELECT(ID_DIR_PMTAB2TAB2)
 IF(NBC.LE.0)K=0
 CALL WDIALOGFIELDSTATE(ID_GENCOPY,K)

 !## fields on quickview
 CALL WDIALOGSELECT(ID_DIR_PMTAB2TAB3)
 CALL WDIALOGFIELDSTATE(IDF_MENU1,J)
! CALL WDIALOGFIELDSTATE(IDF_MENU2,J)
 CALL WDIALOGFIELDSTATE(IDF_LABEL1,J)
 CALL WDIALOGFIELDSTATE(IDF_LABEL2,J)
 CALL WDIALOGFIELDSTATE(ID_APPLY,J)

 !## fill field
 CALL IR1FIELDS_WRITETAB2TAB4()

 !## check plot-checked cells
 CALL WDIALOGSELECT(ID_DIR_PMTAB2)
 CALL WDIALOGFIELDSTATE(IDF_CHECK2,J)
 IF(J.EQ.0)CALL WDIALOGPUTCHECKBOX(IDF_CHECK2,J)
 IF(J.EQ.1)CALL WDIALOGGETCHECKBOX(IDF_CHECK2,J)
 CALL WDIALOGFIELDSTATE(IDF_MENU2,J)

 END SUBROUTINE IR1FIELDS_TAB2

 !###======================================================================
 SUBROUTINE IR1FIELDS_WRITETAB2(IFIELD)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IFIELD
 INTEGER :: I,IMES,IPOL
 REAL :: IMP
 CHARACTER(LEN=500) :: TXT

 CALL WDIALOGSELECT(ID_DIR_PMTAB2TAB1)

 DO IPOL=1,MTREE(IFIELD)%NPOL
  IF(SHPIACT(IPOL).EQ.1)EXIT
 ENDDO

 TXT='Assigned definition for polygon:'//CHAR(13)//CHAR(10)//TRIM(MTREE(IFIELD)%POL(IPOL)%POLNAME)//CHAR(13)//CHAR(10)

 IF(MTREE(IFIELD)%POL(IPOL)%NMES.NE.0)THEN
  !## fill in measures
  DO I=1,MTREE(IFIELD)%POL(IPOL)%NMES
   IMES =MTREE(IFIELD)%POL(IPOL)%MES(I)%IMES
   IMP  =MTREE(IFIELD)%POL(IPOL)%MES(I)%IMP
   TXT=TRIM(TXT)//CHAR(13)//CHAR(10)//TRIM(IR(IMES)%NAMEIR)//': '//TRIM(RTOS(IMP,'F',2))!//' meter'
  END DO
 ELSE
  TXT='No measures are specified yet!'
 ENDIF

 CALL WDIALOGPUTSTRING(IDF_STRING1,TXT)

 END SUBROUTINE IR1FIELDS_WRITETAB2

 !###======================================================================
 SUBROUTINE IR1FIELDS_WRITETAB2TAB4()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,IIR,N,ITREE,IFIELD
 REAL :: LLIMP,ULIMP
 CHARACTER(LEN=500) :: TXT

 !## get level of treeview
 CALL IR1GETTREEVIEWID(ITREE,IFIELD)
 N=MTREE(IFIELD)%NOPT

 !## add constraints always sustainable
 CALL WDIALOGSELECT(ID_DIR_PMTAB2TAB4)
 CALL WDIALOGFIELDSTATE(ID_ADD,1)

 I=0
 IF(N.GT.0)I=1
 CALL WDIALOGFIELDSTATE(ID_REPORT,I)
 CALL WDIALOGFIELDSTATE(ID_INVERSEIR,I)
 CALL WDIALOGFIELDSTATE(IDF_STRING1,1)

 IF(N.GT.0)THEN
  TXT='Defined Constraints:'//CHAR(13)//CHAR(10)
  !## fill in measures
  DO I=1,N
   IIR  =MTREE(IFIELD)%OPT(I)%ISEL
   IF(MTREE(IFIELD)%OPT(I)%IFIXED.EQ.0)THEN
    LLIMP=MTREE(IFIELD)%OPT(I)%LLIMP
    ULIMP=MTREE(IFIELD)%OPT(I)%ULIMP
    TXT=TRIM(TXT)//CHAR(13)//CHAR(10)//TRIM(IR(IIR)%NAMEIR)//': '// &
        TRIM(RTOS(LLIMP,'F',2))//' - '//TRIM(RTOS(ULIMP,'F',2)) ! meter'
   ELSE
    LLIMP=MTREE(IFIELD)%OPT(I)%IMP
    TXT=TRIM(TXT)//CHAR(13)//CHAR(10)//TRIM(IR(IIR)%NAMEIR)//' (fixed): '//TRIM(RTOS(LLIMP,'F',2))
   ENDIF
!       TRIM(RTOS(LLIMP*IR(IIR)%MAXIR,'F',2))//'-'//TRIM(RTOS(ULIMP*IR(IIR)%MAXIR,'F',2))//')'! meter'
  END DO
 ELSE
  TXT='No constraints are specified yet!'
 ENDIF

 CALL WDIALOGPUTSTRING(IDF_STRING1,TXT)

 END SUBROUTINE IR1FIELDS_WRITETAB2TAB4

 !###======================================================================
 SUBROUTINE IR1FIELDS_TAB3TAB1()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: ITREE,IFIELD,I,J,IPER,IRES

 CALL IR1DIRNAME(DIRNAME)

 CALL WDIALOGSELECT(ID_DIR_PMTAB3TAB1)
 CALL IR1FIELDS_DEALLOCATE()
 CALL UTL_IMODFILLMENU(IDF_MENU1,DIRNAME,'*.IDF','F',NLIST,0,1)

 IF(NLIST.EQ.0)THEN
  CALL WDIALOGPUTSTRING(IDF_LABEL1,'No result found for current tree')
  CALL WDIALOGCLEARFIELD(IDF_PICTURE1)
  CALL WDIALOGFIELDSTATE(IDF_MENU1,0)
  CALL WDIALOGFIELDSTATE(IDF_LABEL2,0)
  CALL WDIALOGCLEARFIELD(IDF_PICTURE1)
  CALL WDIALOGSELECT(ID_DIR_PMTAB3)
  CALL WDIALOGTABSTATE(IDF_TAB1,ID_DIR_PMTAB3TAB2,0)
  !## draw current polygon
  CALL POLYGON1DRAWSHAPE(1,SHPNO)
 ELSE
  CALL WDIALOGFIELDSTATE(IDF_MENU1,1)
  !## remove non-recognizable idf"s
  J=0
  DO I=1,NLIST
   CALL IR1FIELDS_GETIPERIRES(LISTNAME(I),IPER,IRES)
!WRITE(*,*) i,j,TRIM(listname(i)),iper,ires,nper,nres
!WRITE(*,*) IPER.LE.NPER.AND.IRES.LE.NRES
   IF(IPER.LE.NPER.AND.IRES.LE.NRES)THEN
    J=J+1
    LISTNAME(J)=LISTNAME(I)
   ENDIF
  ENDDO
  NLIST=J
!WRITE(*,*) NLIST
  !## add path to string listname
  ALLOCATE(IDFRESLIST(NLIST))
  DO I=1,NLIST
   J=INDEX(LISTNAME(I),'.',.TRUE.)-1
   !## remove extent ".idf"
   LISTNAME(I)=LISTNAME(I)(:J)
!WRITE(*,*) TRIM(LISTNAME(I))
   IDFRESLIST(I)=TRIM(DIRNAME)//'\'//LISTNAME(I)(:J)
!WRITE(*,*) TRIM(IDFRESLIST(I))
  ENDDO
  CALL WDIALOGPUTSTRING(IDF_LABEL1,'Select one of the results below:')
  CALL IR1GETTREEVIEWID(ITREE,IFIELD)
  IF(ITREE.NE.3)THEN
   WRITE(*,*) 'ITREE.NEQ.3 IN IR1FIELDS_TAB3TAB1'
  ENDIF
  J=RTREE(IFIELD)%IMENU
!WRITE(*,*) J,RTREE(IFIELD)%IMENU
  IF(J.LE.0)J=1
  IF(J.GT.NLIST)J=NLIST
  CALL WDIALOGSELECT(ID_DIR_PMTAB3TAB1)
!WRITE(*,*) NLIST,J
  CALL WDIALOGPUTMENU(IDF_MENU1,LISTNAME,NLIST,J)!J)!1)
  CALL IR1FIELDS_TAB3_PLOTRES()
 ENDIF

 END SUBROUTINE IR1FIELDS_TAB3TAB1

 !###======================================================================
 SUBROUTINE IR1FIELDS_TAB3TAB2()
 !###======================================================================
 IMPLICIT NONE
! INTEGER :: ITREE,IFIELD
 LOGICAL :: LEX

 !## construct result name
 CALL IR1DIRNAME(DIRNAME)

 !## check whether there are files existing ...
 IF(NLIST.GT.0)THEN
  IDFNAME=TRIM(DIRNAME)//'\DIFF_TARGET.IDF'
  INQUIRE(FILE=IDFNAME,EXIST=LEX)
  IF(LEX)THEN
   CALL IDFINIT(IDFNAMEGIVEN=IDFNAME,LEGNAME=TRIM(TARGETLEG))
   !## draw legend
   CALL LEGPLOT_MAIN(ID_DIR_PMTAB3TAB2,IDF_PICTURE1,4)  !## three columns
  ELSE
   !## compute it
   CALL IR1FIELDS_TAB3_CALCDIFF()
!  CALL WDIALOGSELECT(ID_DIR_PMTAB3TAB2)
!  CALL WDIALOGCLEARFIELD(IDF_PICTURE1)
  !## draw current polygon
!  CALL POLYGON1DRAWSHAPE(1,SHPNO)
  ENDIF
 ELSE
  CALL IR1DELETETARGETIDF()
 ENDIF

 !## write results
 CALL IR1FIELDS_WRITETAB3TAB2()

 END SUBROUTINE IR1FIELDS_TAB3TAB2

 !###======================================================================
 SUBROUTINE IR1DELETETARGETIDF()
 !###======================================================================
 IMPLICIT NONE
 LOGICAL :: LEX

 !## construct result name
 CALL IR1DIRNAME(DIRNAME)

 INQUIRE(FILE=TRIM(DIRNAME)//'\DIFF_TARGET.IDF',EXIST=LEX)
 IF(LEX)CALL IOSDELETEFILE(TRIM(DIRNAME)//'\DIFF_TARGET.IDF')

 END SUBROUTINE IR1DELETETARGETIDF

 !###======================================================================
 SUBROUTINE IR1FIELDS_WRITETAB3TAB2()!IFIELD)
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,INEWT,INEWP,IPOL,IFIELD,ITREE,ITARGET,IMEASURE
 REAL :: LOWER,UPPER
 CHARACTER(LEN=500) :: TXT

 !## get level of treeview
 CALL IR1GETTREEVIEWID(ITREE,IFIELD)
 CALL IR1GETTREEIDS(IFIELD,ITARGET,IMEASURE)
 IFIELD=ITARGET

 CALL WDIALOGSELECT(ID_DIR_PMTAB3TAB2)

 !## one polygon selected, present results
 IF(SUM(SHPIACT(1:SHPNO)).EQ.1)THEN
  DO IPOL=1,TTREE(IFIELD)%NPOL
   IF(SHPIACT(IPOL).EQ.1)EXIT
  ENDDO

  IF(TTREE(IFIELD)%POL(IPOL)%NDEF.NE.0)THEN
   TXT='Activated targets:'//CHAR(13)//CHAR(10)
   !## fill in measures
   DO I=1,TTREE(IFIELD)%POL(IPOL)%NDEF
    INEWT=TTREE(IFIELD)%POL(IPOL)%DEF(I)%INEWT
    INEWP=TTREE(IFIELD)%POL(IPOL)%DEF(I)%INEWP
    LOWER=TTREE(IFIELD)%POL(IPOL)%DEF(I)%LOWER
    UPPER=TTREE(IFIELD)%POL(IPOL)%DEF(I)%UPPER
    TXT=TRIM(TXT)//CHAR(13)//CHAR(10)//TRIM(PER(INEWP)%NAMEPER)//' '//TRIM(RES(INEWT)%NAMERES)

    !## heads
    IF(RES(INEWT)%ITYPERES.EQ.0)THEN
     TXT=TRIM(TXT)//& 
         ': mean head '//TRIM(RTOS(TTREE(IFIELD)%POL(IPOL)%DEF(I)%MEAN,'F',2))// &
         ' should be in between '//TRIM(RTOS(LOWER,'F',2))//' and '//TRIM(RTOS(UPPER,'F',2))
!     TXT=TRIM(TXT)//&
!         ': '//TRIM(ITOS(TTREE(IFIELD)%POL(IPOL)%DEF(I)%REFFECT_LO))//'% of area has effect >'//TRIM(RTOS(LOWER,'F',2))// &
!         '; '//TRIM(ITOS(TTREE(IFIELD)%POL(IPOL)%DEF(I)%REFFECT_UP))//'% of area has effect <'//TRIM(RTOS(UPPER,'F',2))// &
!         ' (target: '//TRIM(ITOS(TTREE(IFIELD)%POL(IPOL)%EFFECT))//'%)'
    !## m/dag (mean)
    ELSEIF(RES(INEWT)%ITYPERES.EQ.1)THEN
     TXT=TRIM(TXT)//& 
         ': mean flux '//TRIM(RTOS(TTREE(IFIELD)%POL(IPOL)%DEF(I)%MEAN,'F',2))// &
         ' should be in between '//TRIM(RTOS(LOWER,'F',2))//'and '//TRIM(RTOS(UPPER,'F',2))
    ENDIF

   END DO
  ELSE
   TXT='No targets are specified yet!'
  ENDIF
 ELSE
  TXT='Select one target polygon for which result should be presented!'
 ENDIF

 CALL WDIALOGPUTSTRING(IDF_STRING1,TXT)

 END SUBROUTINE IR1FIELDS_WRITETAB3TAB2

 !###======================================================================
 SUBROUTINE IR1FIELDS_TAB3_PLOTRES()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,IPER,IRES,ITREE,IFIELD

 CALL IR1GETTREEVIEWID(ITREE,IFIELD)
 IF(ITREE.NE.3)THEN
  WRITE(*,*) 'ITREE.NEQ.3 IN IR1FIELDS_TAB3TAB1'
 ENDIF

 CALL WDIALOGSELECT(ID_DIR_PMTAB3TAB1)
 CALL WDIALOGGETMENU(IDF_MENU1,I)
 !## strore position in menu-field
 RTREE(IFIELD)%IMENU=I

 !## get type of idf, try to recognize it, if not so block target-tab
 J=INDEX(LISTNAME(I),'\',.TRUE.)+1
 IDFNAME=UTL_CAP(IDFRESLIST(I)(J:),'U')

 CALL IR1FIELDS_GETIPERIRES(IDFNAME,IPER,IRES)

 !## plot idf in imod
 IF(IRES.LE.NRES)THEN
  CALL IDFINIT(IDFNAMEGIVEN=TRIM(IDFRESLIST(I))//'.IDF',LEGNAME=TRIM(RES(IRES)%LEGRES))
  J=1
 ELSE
  CALL IDFINIT(IDFNAMEGIVEN=TRIM(IDFRESLIST(I))//'.IDF') !,'')
  J=0
 ENDIF

 IF(IPER.GT.NPER.AND.IRES.GT.NRES)J=0
 CALL IR1FIELDS_EVALUATETARGET(J,I)

 CALL WDIALOGSELECT(ID_DIR_PMTAB3)
 CALL WDIALOGTABSTATE(IDF_TAB1,ID_DIR_PMTAB3TAB2,J)

 !## draw legend
 CALL LEGPLOT_MAIN(ID_DIR_PMTAB3TAB1,IDF_PICTURE1,3)  !## three columns

 END SUBROUTINE IR1FIELDS_TAB3_PLOTRES

 !###======================================================================
 SUBROUTINE IR1FIELDS_PLOTLABELUNIT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,IRES,IPER
 CHARACTER(LEN=50) :: CNAME

 CALL WDIALOGGETMENU(IDF_MENU1,I,CNAME)
! CALL IR1FIELDS_GETIPERIRES(PERRES(I),IPER,IRES)
 CALL IR1FIELDS_GETIPERIRES(CNAME,IPER,IRES)

 IF(RES(IRES)%ITYPERES.EQ.0)THEN
  CALL WDIALOGPUTSTRING(IDF_LABEL3,'meter')
 ELSEIF(RES(IRES)%ITYPERES.EQ.1)THEN
  CALL WDIALOGPUTSTRING(IDF_LABEL3,'mm/day')
 ENDIF

 END SUBROUTINE IR1FIELDS_PLOTLABELUNIT

 !###======================================================================
 SUBROUTINE IR1FIELDS_EVALUATETARGET(IOKAY,ITARGET)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(INOUT) :: IOKAY
 INTEGER,INTENT(OUT) :: ITARGET
 INTEGER :: IPOL,IFIELD,ITREE,IMEASURE,NDEF

 !## check whether targets are available AND contains current effect
 IF(IOKAY.EQ.0)RETURN

 !## get level of treeview
 CALL IR1GETTREEVIEWID(ITREE,IFIELD)
 !## get id's for results/measures
 CALL IR1GETTREEIDS(IFIELD,ITARGET,IMEASURE)
 !## no polygons available
 IF(TTREE(ITARGET)%NPOL.LE.0)THEN
  CALL WDIALOGSELECT(ID_DIR_PMTAB3TAB1)
  CALL WDIALOGCOLOUR(IDF_LABEL2,-1,WRGB(255,128,128))
  CALL WDIALOGPUTSTRING(IDF_LABEL2,'No polygons found to release target computation')
  IOKAY=0
 ELSE
  !## check existence in target definitions - of current iper/ires
  NDEF=0
  DO IPOL=1,TTREE(ITARGET)%NPOL
   IF(TTREE(ITARGET)%POL(IPOL)%NDEF.GT.0)NDEF=NDEF+1
  END DO
  IF(NDEF.EQ.0)THEN
   CALL WDIALOGSELECT(ID_DIR_PMTAB3TAB1)
   CALL WDIALOGCOLOUR(IDF_LABEL2,-1,WRGB(255,128,128))
   CALL WDIALOGPUTSTRING(IDF_LABEL2,'No corresponding targets defined to release target computation')
   IOKAY=0
  ENDIF
 ENDIF

 IF(IOKAY.EQ.1)THEN
  CALL WDIALOGSELECT(ID_DIR_PMTAB3TAB1)
  CALL WDIALOGCOLOUR(IDF_LABEL2,-1,WRGB(128,255,128))
  CALL WDIALOGPUTSTRING(IDF_LABEL2,'Target will be evaluated whenever changed to the tab: [target]')
 ENDIF

 END SUBROUTINE IR1FIELDS_EVALUATETARGET

 !###======================================================================
 SUBROUTINE IR1FIELDS_TAB3_CALCDIFF()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,ILIST,IPOL,ITARGET,IDEF,IROW,ICOL,MXCRD,IC1,IC2,IR1,IR2,IPER,IRES
 REAL :: X,Y,XX,YY,XRESULT,XMIN,YMIN,XMAX,YMAX

 I=1
 CALL IR1FIELDS_EVALUATETARGET(I,ITARGET)
 IF(I.EQ.0)RETURN

 !## refresh list of idf's
 CALL IR1FIELDS_TAB3TAB1()

 !## copy data - SHAPES -> POL
 CALL IR1SHAPE2POL(1,ITARGET)
 !## restore them again ...
 CALL IR1POL2SHAPE(1,ITARGET)

 DO IPOL=1,TTREE(ITARGET)%NPOL; TTREE(ITARGET)%POL(IPOL)%IACT=1; ENDDO
 IF(.NOT.IR2GETEXTENSION(1,ITARGET,XMIN,YMIN,XMAX,YMAX,MXCRD))RETURN

 CALL UTL_MESSAGEHANDLE(0)

 ALLOCATE(IDFDIFF(2)); DO I=1,SIZE(IDFDIFF); CALL IDFNULLIFY(IDFDIFF(I)); ENDDO

 !## perform each polygon
 DO IPOL=1,TTREE(ITARGET)%NPOL
  !## initialize ALL realized effectiveness
  DO IDEF=1,TTREE(ITARGET)%POL(IPOL)%NDEF
   TTREE(ITARGET)%POL(IPOL)%DEF(IDEF)%MEAN      =0.0
   TTREE(ITARGET)%POL(IPOL)%DEF(IDEF)%POLSIZE   =0
  ENDDO
 ENDDO

 !## process all idf from the result tab
 DO ILIST=1,NLIST

  CALL IR1FIELDS_GETIPERIRES(TRIM(IDFRESLIST(ILIST)),IPER,IRES)

  !## correct recognized file
  IF(IPER.LE.NPER.AND.IRES.LE.NRES)THEN

   !## result idf
   IF(.NOT.IDFREAD(IDFDIFF(1),TRIM(IDFRESLIST(ILIST))//'.IDF',0))THEN
    CALL IR1FIELDS_DEALLOCATECALC()
    RETURN
   ENDIF

   !## perform each polygon
   DO IPOL=1,TTREE(ITARGET)%NPOL

    !## turn on polygon, turn the others off
    DO I=1,TTREE(ITARGET)%NPOL; TTREE(ITARGET)%POL(I)%IACT=0; ENDDO
    TTREE(ITARGET)%POL(IPOL)%IACT   =1  !## turn current polygon on only

    !## get extension of current polygon to speed up computation
    IF(IR2GETEXTENSION(1,ITARGET,XMIN,YMIN,XMAX,YMAX,MXCRD))THEN

     !## get domain fo polygons
     CALL IDFIROWICOL(IDFDIFF(1),IR1,IC1,XMIN,YMAX)
     CALL IDFIROWICOL(IDFDIFF(1),IR2,IC2,XMAX,YMIN)

     DO IDEF=1,TTREE(ITARGET)%POL(IPOL)%NDEF
      !## check definition that fits current result type
      IF(TTREE(ITARGET)%POL(IPOL)%DEF(IDEF)%INEWP.EQ.IPER.AND.&
         TTREE(ITARGET)%POL(IPOL)%DEF(IDEF)%INEWT.EQ.IRES)THEN

       DO IROW=IR1,IR2
        DO ICOL=IC1,IC2
         !## get xy-coordinates
         CALL IDFGETLOC(IDFDIFF(1),IROW,ICOL,X,Y)
         !## inside polygon?
         IF(UTL_INSIDEPOLYGON(X,Y,TTREE(ITARGET)%POL(IPOL)%X,TTREE(ITARGET)%POL(IPOL)%Y, &
            TTREE(ITARGET)%POL(IPOL)%NCRD).EQ.1)THEN

          !## get value
          XRESULT=IDFGETVAL(IDFDIFF(1),IROW,ICOL)
          !## count polygon size
          TTREE(ITARGET)%POL(IPOL)%DEF(IDEF)%POLSIZE=TTREE(ITARGET)%POL(IPOL)%DEF(IDEF)%POLSIZE+1

          !## heads
          IF(RES(IRES)%ITYPERES.EQ.0)THEN
           !## evaluate value to target ...
           TTREE(ITARGET)%POL(IPOL)%DEF(IDEF)%MEAN=TTREE(ITARGET)%POL(IPOL)%DEF(IDEF)%MEAN+XRESULT
          !## m/dag (mean)
          ELSEIF(RES(IRES)%ITYPERES.EQ.1)THEN
           TTREE(ITARGET)%POL(IPOL)%DEF(IDEF)%MEAN=TTREE(ITARGET)%POL(IPOL)%DEF(IDEF)%MEAN+XRESULT
          ENDIF

         ENDIF
        ENDDO
       ENDDO

      ENDIF
     ENDDO
    ENDIF

   ENDDO
   CLOSE(IDFDIFF(1)%IU)

  ENDIF
 ENDDO

 !## construct effectiveness for each target polygon for each target definition within
 DO IPOL=1,TTREE(ITARGET)%NPOL
  DO IDEF=1,TTREE(ITARGET)%POL(IPOL)%NDEF

   IRES=TTREE(ITARGET)%POL(IPOL)%DEF(IDEF)%INEWT
   YY  =REAL(TTREE(ITARGET)%POL(IPOL)%DEF(IDEF)%POLSIZE)

   !## heads (mean)
   IF(RES(IRES)%ITYPERES.EQ.0)THEN

    XX= TTREE(ITARGET)%POL(IPOL)%DEF(IDEF)%MEAN
    XX= XX/YY
    TTREE(ITARGET)%POL(IPOL)%DEF(IDEF)%MEAN=XX

   !## m/dag (mean)
   ELSEIF(RES(IRES)%ITYPERES.EQ.1)THEN

    XX= TTREE(ITARGET)%POL(IPOL)%DEF(IDEF)%MEAN
    XX= XX/YY
    TTREE(ITARGET)%POL(IPOL)%DEF(IDEF)%MEAN=XX

   ENDIF

  ENDDO
 ENDDO

 !## difference will be similar to result
 CALL IDFCOPY(IDFDIFF(1),IDFDIFF(2))
 !## initialize x()
 IDFDIFF(2)%NODATA=-999.99
 IDFDIFF(2)%X     = 0.0

 ALLOCATE(IP(IDFDIFF(2)%NCOL,IDFDIFF(2)%NROW))
 IP=INT(0,1)

 !## post process targets to be filled in for the entire polygon
 DO IROW=1,IDFDIFF(2)%NROW
  DO ICOL=1,IDFDIFF(2)%NCOL
   !## get xy-coordinates
   CALL IDFGETLOC(IDFDIFF(1),IROW,ICOL,X,Y)
   !## perform each polygon
   DO IPOL=1,TTREE(ITARGET)%NPOL
    !## inside polygon?
    IF(UTL_INSIDEPOLYGON(X,Y,TTREE(ITARGET)%POL(IPOL)%X,TTREE(ITARGET)%POL(IPOL)%Y,TTREE(ITARGET)%POL(IPOL)%NCRD).EQ.1)THEN
     IP(ICOL,IROW)=INT(1,1)
     DO IDEF=1,TTREE(ITARGET)%POL(IPOL)%NDEF

      IRES=TTREE(ITARGET)%POL(IPOL)%DEF(IDEF)%INEWT

      XX=TTREE(ITARGET)%POL(IPOL)%DEF(IDEF)%MEAN
      IF(ABS(XX-TTREE(ITARGET)%POL(IPOL)%DEF(IDEF)%LOWER).LE.0.01.AND. &
         ABS(XX-TTREE(ITARGET)%POL(IPOL)%DEF(IDEF)%UPPER).LE.0.01)THEN
       XX=100.0/REAL(TTREE(ITARGET)%POL(IPOL)%NDEF) !## to make it 100%
      ELSE
       XX=0.0
      ENDIF
      IDFDIFF(2)%X(ICOL,IROW)=IDFDIFF(2)%X(ICOL,IROW)+XX

     ENDDO
    ENDIF
   ENDDO
  ENDDO
 ENDDO

 !## post process targets to be filled in for the entire polygon
 DO IROW=1,IDFDIFF(2)%NROW
  DO ICOL=1,IDFDIFF(2)%NCOL
   IF(IP(ICOL,IROW).EQ.INT(0,1))THEN
    IDFDIFF(2)%X(ICOL,IROW)=IDFDIFF(2)%NODATA
   ENDIF
  ENDDO
 ENDDO

 !## construct result name
 CALL IR1DIRNAME(DIRNAME)

 !## write result
 IDFNAME=TRIM(DIRNAME)//'\DIFF_TARGET.IDF'
 IF(.NOT.IDFWRITE(IDFDIFF(2),IDFNAME,1))THEN
 ENDIF
 !## deallocate memory
 CALL IR1FIELDS_DEALLOCATECALC()
 !## plot result
 CALL IDFINIT(IDFNAMEGIVEN=IDFNAME,LEGNAME=TRIM(TARGETLEG))
 !## draw legend
 CALL LEGPLOT_MAIN(ID_DIR_PMTAB3TAB2,IDF_PICTURE1,4)  !## three columns
 !## write results
 CALL IR1FIELDS_WRITETAB3TAB2()

 END SUBROUTINE IR1FIELDS_TAB3_CALCDIFF

 !###======================================================================
 SUBROUTINE IR1FIELDS_DEALLOCATECALC()
 !###======================================================================
 IMPLICIT NONE

 IF(ALLOCATED(IDFDIFF))THEN
  CALL IDFDEALLOCATE(IDFDIFF,SIZE(IDFDIFF))
  DEALLOCATE(IDFDIFF)
 ENDIF

 IF(ALLOCATED(IP))DEALLOCATE(IP)

 CALL UTL_MESSAGEHANDLE(1)

 END SUBROUTINE IR1FIELDS_DEALLOCATECALC

 !###======================================================================
 SUBROUTINE IR1FIELDS_DEALLOCATE()
 !###======================================================================
 IMPLICIT NONE

 IF(ALLOCATED(LISTNAME))DEALLOCATE(LISTNAME)
 IF(ALLOCATED(IDFRESLIST))DEALLOCATE(IDFRESLIST)

 END SUBROUTINE IR1FIELDS_DEALLOCATE

END MODULE MOD_IR_FIELDS
