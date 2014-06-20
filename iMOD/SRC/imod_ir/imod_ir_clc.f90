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
MODULE MOD_IR_CLC

USE WINTERACTER
USE MOD_UTL, ONLY : ITOS,UTL_IDFSNAPTOGRID,UTL_WAITMESSAGE,UTL_GETUNIQUE,UTL_CREATEDIR,UTL_MESSAGEHANDLE,&
    UTL_GETUNIT,UTL_INSIDEPOLYGON
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_IDF, ONLY : IDFREAD,IDFOPEN,IDFREADDIM,IDFGETVAL,IDFDEALLOCATEX, &
                    IDFALLOCATEX,IDFALLOCATESXY,IDFWRITE,IDFIROWICOL,IDFGETLOC
USE MOD_IR_PAR
USE MOD_IR_UTL, ONLY : IR1GETTREEVIEWID,IR1SHAPE2POL,IR1POL2SHAPE,IR1FIELDS_STRING,IR1GETTREEID,IR1FIELDS_GETIPERIRES, &
                       IR1IMPULSEFACTOR
USE MOD_IR_LINEAR, ONLY : IR1LINEARPROGRAMMING,IR2LINEAR_PERCENTILES,IR1LINEAR_ALLOCATE,IR1LINEAR_DEALLOCATE,IR1LINEAR_SHOWOUTPUT
USE MOD_SMPLX, ONLY : LPSTATUS,ICNVG
USE MODPLOT, ONLY : MXMPLOT,MP
USE MOD_LEGPLOT, ONLY : LEGPLOT_MAIN
USE MOD_OSD, ONLY : OSD_OPEN
USE IMOD

USE MOD_POLYGON_PAR, ONLY : SHPNO

INTEGER,ALLOCATABLE,DIMENSION(:),PRIVATE :: IACT

CONTAINS

 !###======================================================================
 SUBROUTINE IR1COMPUTEIR(LQUICK,LINVIR)
 !###======================================================================
 IMPLICIT NONE
 LOGICAL,INTENT(IN) :: LQUICK,LINVIR
 INTEGER :: ITREE,JTREE,IFIELD,JFIELD,NCOMP,IFT,INEXT
 CHARACTER(LEN=MAXLEN),DIMENSION(3) :: CTREE
 INTEGER :: IPOL,JPOL,IIR,JIR,NPOL,NPOL1,NPOL2,IOS,I,J,IPER,IRES,ITYPE
 TYPE(WIN_MESSAGE) :: MESSAGE

 !LQUICK=.TRUE. : process IR selected times/results
 !      =.FALSE.: process IR for all times/results (in combination with linvir=.true. process only
 !                those that are selected in dialog
 !LINVIR=.TRUE. : determine strength for measurements thru linear-programming
 !      =.FALSE.: apply given strengths

 CALL UTL_MESSAGEHANDLE(0)

 !## get level of treeview
 CALL IR1GETTREEVIEWID(ITREE,IFIELD)

 !## get name
 CALL IR1FIELDS_STRING(CTREE,ITREE,IFIELD)

 NPOL=MTREE(IFIELD)%NPOL
 ALLOCATE(IACT(NPOL))
 !## temporary copy measure polygon setting, to be restored afterwards
 IACT(1:NPOL)=MTREE(IFIELD)%POL(1:NPOL)%IACT

 !## generate results - adjust ifield
 IF(.NOT.LINVIR.AND..NOT.LQUICK)THEN
  !## mother id, (id of measure)
  I=RTREE(IFIELD)%IDPOS
  CALL IR1GETTREEID(ITREE,IFIELD,I)
  DO I=1,MTREE(IFIELD)%NPOL; MTREE(IFIELD)%POL(I)%IACT=1; END DO
 ELSE
  !## copy data of current measures
  CALL IR1SHAPE2POL(ITREE,IFIELD)
  !## restore them again ...
  CALL IR1POL2SHAPE(ITREE,IFIELD)

  I=MTREE(IFIELD)%IDPOS
  CALL IR1GETTREEID(JTREE,JFIELD,I)
 ENDIF

 ALLOCATE(IPR(NPER),IRS(NRES))

 !## initialise ipr/irs - pointer to determine whether topic and time are active
 IPR=0
 IRS=0

 IF(LQUICK)THEN
  CALL WDIALOGSELECT(ID_DIR_PMTAB2TAB3)
  CALL WDIALOGGETMENU(IDF_MENU1,I)
  CALL IR1FIELDS_GETIPERIRES(PERRES(I),IPER,IRES)
  !## process only those times/results that are selected
  IF(LINVIR)THEN

   !## turn selected in target polygons result/times on only
   DO IPOL=1,TTREE(JFIELD)%NPOL
    IF(TTREE(JFIELD)%POL(IPOL)%NDEF.EQ.0)THEN
     CALL IR2DEALLOCATE()
     CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'There is no target defined for polygon number: '//TRIM(ITOS(IPOL))// &
       ', named: '//TRIM(TTREE(JFIELD)%POL(IPOL)%POLNAME),'Error')
     RETURN
    ENDIF
    DO I=1,TTREE(JFIELD)%POL(IPOL)%NDEF
     J     =TTREE(JFIELD)%POL(IPOL)%DEF(I)%INEWT
     IRS(J)=1  !## topic
     J     =TTREE(JFIELD)%POL(IPOL)%DEF(I)%INEWP
     IPR(J)=1  !## period
    ENDDO
   ENDDO

  ELSE
   !## turn selected result/times on only
   IPR(IPER)=1
   IRS(IRES)=1
  ENDIF
 ELSE
  !## turn all result/times on
  IPR=1
  IRS=1
 ENDIF

! WRITE(*,*) 'IPR',IPR
! WRITE(*,*) 'IRS',IRS
!PAUSE
 !## default
 IF(.NOT.LINVIR)THEN

  IF(.NOT.LQUICK)THEN
   CALL WDIALOGLOAD(ID_DIRPROGRESS,ID_DIRPROGRESS)
   CALL WDIALOGPUTSTRING(IDF_LABEL1,'Initializing ...')
   CALL WDIALOGRANGEPROGRESSBAR(IDF_PROGRESS1,0,SUM(IRS)*SUM(IPR))!*2)
   CALL WDIALOGPUTPROGRESSBAR(IDF_PROGRESS1,0,ABSOLUTE)
   CALL WDIALOGSHOW(-1,-1,0,3)
  ENDIF

  IF(.NOT.IR2COMPUTEIRMAIN(IFIELD,JFIELD,CTREE,LQUICK,0,0))THEN
   !## error occured
  ENDIF

  IF(.NOT.LQUICK)THEN
   CALL WDIALOGSELECT(ID_DIRPROGRESS)
   CALL WDIALOGUNLOAD()
  ENDIF

 !## apply inverse IR computation - set measurement on true one-by-one
 ELSE

  !## target polygons
  IF(.NOT.IR1CHECKPOLYGON(1,JFIELD))RETURN
  !## measurement polygons
  IF(.NOT.IR1CHECKPOLYGON(2,IFIELD))RETURN

  !## process all polygons
  NPOL1=IR1COMPUTEIR_NPOL(1,JFIELD)
  NPOL2=IR1COMPUTEIR_NPOL(2,IFIELD)

  !## fill ir()%isel,ir()%llimp,ir()%ulimp
  IR%ISEL=0
  DO J=1,MTREE(IFIELD)%NOPT
   I=MTREE(IFIELD)%OPT(J)%ISEL
   IR(I)%ISEL  =1
   IR(I)%LLIMP =MTREE(IFIELD)%OPT(J)%LLIMP
   IR(I)%ULIMP =MTREE(IFIELD)%OPT(J)%ULIMP
   IR(I)%IFIXED=MTREE(IFIELD)%OPT(J)%IFIXED
   IR(I)%IMP   =MTREE(IFIELD)%OPT(J)%IMP
   !## binary ir type - potential fixed
   IF(IR(I)%TYPEIR.EQ.1.AND.IR(I)%IFIXED.EQ.0)IR(I)%IFIXED=-1
  ENDDO

  JIR=0
  DO I=1,NIR
   IF(IR(I)%ISEL.NE.0)JIR=JIR+1
  END DO

  !## turn all seleced polygons on to reflect selected measure in optimize dialog
  DO IPOL=1,MTREE(IFIELD)%NPOL
   !## make sure all selected measures are within current state
   MTREE(IFIELD)%POL(IPOL)%NMES=JIR
   !## make sure memory is allocated to store def
   IF(.NOT.ASSOCIATED(MTREE(IFIELD)%POL(IPOL)%MES))THEN
    ALLOCATE(MTREE(IFIELD)%POL(IPOL)%MES(MAXMES))
   ENDIF
   J=0
   DO IIR=1,NIR
    !## process only these that are selected
    IF(IR(IIR)%ISEL.NE.0)THEN
     J=J+1
     MTREE(IFIELD)%POL(IPOL)%MES(J)%IMES=IIR
     MTREE(IFIELD)%POL(IPOL)%MES(J)%IMP =0.0!IIR
    ENDIF
   ENDDO
  ENDDO

  !## allocate matrix to store coefficients
  ALLOCATE(COEF(JIR,NPOL1,NPOL2,SUM(IRS),SUM(IPR),2),STAT=IOS)
  IF(IOS.NE.0)THEN
   CALL IR2DEALLOCATE()
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error allocating memory to store coefficients','Error')
   RETURN
  ENDIF

  CALL WDIALOGLOAD(ID_DIRPROGRESS,ID_DIRPROGRESS)
  CALL WDIALOGPUTSTRING(IDF_LABEL1,'Initializing ...')
  CALL WDIALOGRANGEPROGRESSBAR(IDF_PROGRESS1,0,NPOL2*JIR)!*SUM(IRS))!*SUM(IPR))!*2)
  CALL WDIALOGPUTPROGRESSBAR(IDF_PROGRESS1,0,ABSOLUTE)
  CALL WDIALOGSHOW(-1,-1,0,3)

  !## initialise coefficients
  COEF=0.0

  JIR =0
  !## loop over available impulses (selected only!)
IIRLOOP: DO IIR=1,NIR

   !## process only these that are selected
   IF(IR(IIR)%ISEL.NE.0)THEN
    JIR=JIR+1

    !## loop over measure polygons
    JPOL=0
    DO IPOL=1,MTREE(IFIELD)%NPOL
     !## turn all measure polygons off
     MTREE(IFIELD)%POL(1:NPOL2)%IACT=0
     !## turn one-by-one polygons on
     MTREE(IFIELD)%POL(IPOL)%IACT=1
     !## turn all impulses off
     MTREE(IFIELD)%POL(IPOL)%MES(1:NIR)%IMP=0.0
     !## turn appropriate impulse on, only!
     MTREE(IFIELD)%POL(IPOL)%MES(JIR)%IMP=IR(IIR)%MAXIR
     JPOL=JPOL+1

     CALL WDIALOGSELECT(ID_DIRPROGRESS)
     CALL WDIALOGPUTSTRING(IDF_LABEL2,'Measure: '//TRIM(IR(IIR)%NAMEIR))
     CALL WDIALOGPUTSTRING(IDF_LABEL3,'Polygon: '//TRIM(MTREE(IFIELD)%POL(IPOL)%POLNAME))

     CALL WMESSAGEPEEK(ITYPE,MESSAGE)

     !## compute effect
     IF(.NOT.IR2COMPUTEIRMAIN(IFIELD,JFIELD,CTREE,LQUICK,JIR,JPOL))EXIT IIRLOOP
    END DO
   ENDIF
  END DO IIRLOOP

  !## i=1;normal i=2;full throttle
  NCOMP=0

  DO IFT=1,2

   CALL IR1LINEAR_ALLOCATE(IFIELD,JFIELD)

   !## full throttle
   IF(IFT.EQ.2)THEN

    DO J=1,MTREE(IFIELD)%NOPT
     I=MTREE(IFIELD)%OPT(J)%ISEL
     IR(I)%ISEL  =1
     IR(I)%LLIMP =IR(I)%MINIR
     IR(I)%ULIMP =IR(I)%MAXIR
     IR(I)%IFIXED=MTREE(IFIELD)%OPT(J)%IFIXED
     IR(I)%IMP   =MTREE(IFIELD)%OPT(J)%IMP
     !## binary ir type - potential fixed
     IF(IR(I)%TYPEIR.EQ.1.AND.IR(I)%IFIXED.EQ.0)IR(I)%IFIXED=-1
    ENDDO
   ENDIF

   !## do while unfixed binair become fixed
   DO
    NCOMP=NCOMP+1
    !## start linear programming
    CALL IR1LINEARPROGRAMMING(IFIELD,JFIELD,NCOMP,IFT,INEXT) !## restricted
    !## finished
    IF(INEXT.EQ.0)EXIT
   ENDDO

   CALL IR1LINEAR_DEALLOCATE()

  END DO

  CALL WDIALOGSELECT(ID_DIRPROGRESS)
  CALL WDIALOGUNLOAD()

  IF(ICNVG.NE.0)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Quick-Scan did not find an optimal solution'//CHAR(13)// &
                     TRIM(LPSTATUS),'Warning')
  ENDIF
  CALL IR1LINEAR_SHOWOUTPUT(IFIELD)!,JFIELD)

 ENDIF

 !## reset selection of polygons
 MTREE(IFIELD)%POL(1:NPOL)%IACT=IACT(1:NPOL)

 END SUBROUTINE IR1COMPUTEIR

 !###======================================================================
 INTEGER FUNCTION IR1COMPUTEIR_NPOL(IC,IFIELD)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IC,IFIELD

 IR1COMPUTEIR_NPOL=0
 IF(IC.EQ.1)THEN
  IR1COMPUTEIR_NPOL=TTREE(IFIELD)%NPOL
 ELSEIF(IC.EQ.2)THEN
  IR1COMPUTEIR_NPOL=MTREE(IFIELD)%NPOL
 ENDIF

 END FUNCTION IR1COMPUTEIR_NPOL

 !###======================================================================
 FUNCTION IR1CHECKPOLYGON(IC,IFIELD)
 !###======================================================================
 IMPLICIT NONE
 LOGICAL :: IR1CHECKPOLYGON
 INTEGER,INTENT(IN) :: IC,IFIELD
 INTEGER :: NPOL

 IR1CHECKPOLYGON=.FALSE.

 IF(IC.EQ.1)NPOL=TTREE(IFIELD)%NPOL
 IF(IC.EQ.2)NPOL=MTREE(IFIELD)%NPOL

 IF(NPOL.LE.0)THEN
  IF(IC.EQ.1)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK, &
    'There are no TARGET polygons given for which the Inverse-IR will be computed','Warning')
  IF(IC.EQ.2)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK, &
    'There are no MEASURE polygons given for which the Inverse-IR will be computed','Warning')
  CALL IR2DEALLOCATE()
  RETURN
 ENDIF

 IR1CHECKPOLYGON=.TRUE.

 END FUNCTION IR1CHECKPOLYGON

 !###======================================================================
 FUNCTION IR2COMPUTEIRMAIN(IFIELD,JFIELD,CTREE,LQUICK,IIR,JPOL)
 !###======================================================================
 IMPLICIT NONE
 LOGICAL :: IR2COMPUTEIRMAIN
 LOGICAL,INTENT(IN) :: LQUICK
 INTEGER,INTENT(IN) :: IFIELD,JFIELD,IIR,JPOL
 CHARACTER(LEN=*),INTENT(IN),DIMENSION(3) :: CTREE
 CHARACTER(LEN=256) :: DIRNAME,IDFNAME
 REAL :: XMIN,YMIN,XMAX,YMAX
 LOGICAL :: LINVIR
 INTEGER :: MXCRD,NIDF,IPER,IRES,JPER,JRES
 INTEGER :: ITYPE
 TYPE(WIN_MESSAGE) :: MESSAGE

 IR2COMPUTEIRMAIN=.FALSE.

 LINVIR=.FALSE.
 IF(IIR.GT.0)LINVIR=.TRUE.

 !## get dimensions of impulse within each (selected) polygon
 IF(.NOT.IR2GETEXTENSION(2,IFIELD,XMIN,YMIN,XMAX,YMAX,MXCRD))RETURN

 !## open all neccessary idf with pointers and read headers
 IF(.NOT.IR2READIDFP(IFIELD,XMIN,YMIN,XMAX,YMAX))RETURN

 !## compute all results
 JPER=0
 DO IPER=1,NPER
  IF(IPR(IPER).EQ.1)THEN
   JPER=JPER+1
   JRES=0
   DO IRES=1,NRES
    IF(IRS(IRES).EQ.1)THEN
     JRES=JRES+1

     CALL WMESSAGEPEEK(ITYPE,MESSAGE)

     IF(LINVIR.OR..NOT.LQUICK)THEN
      CALL WDIALOGSELECT(ID_DIRPROGRESS)
      CALL WDIALOGPUTPROGRESSBAR(IDF_PROGRESS1,1,RELATIVE)
      CALL WDIALOGPUTSTRING(IDF_LABEL1,'Computing '//TRIM(PER(IPER)%NAMEPER)//'_'//TRIM(RES(IRES)%NAMERES)//'...')
     ENDIF

     IDFNAME=TRIM(RES(IRES)%DIRRES)//'\'//TRIM(RES(IRES)%DIRRES)//'_s'// &
             TRIM(ITOS(PER(IPER)%IPERPER))//'L'//TRIM(ITOS(RES(IRES)%ILAYRES))//'.IDF'
     !## open all neccessary idf's (within the polygons) and read headers
     IF(.NOT.IR2READIDF(MXCRD,IFIELD,XMIN,YMIN,XMAX,YMAX,LQUICK,NIDF,IDFNAME))RETURN

     !## construct new idf-grid
     IF(.NOT.IR2CREATEIDF(NIDF))RETURN

     !## collect all effects and store them into array a()
     CALL IR2COMPUTEIR(NIDF,RES(IRES)%ITYPERES)

     !## create directory for inverse-computations
     IF(LINVIR)THEN
      DIRNAME=TRIM(PREFVAL(1))//'\tmp\'//TRIM(IR(IIR)%NAMEIR)
     ELSE
      !## store results in tmp map for quick-view
      IF(LQUICK)THEN
       DIRNAME=TRIM(PREFVAL(1))//'\tmp'
      !## store results in ir results map for "permanent" storage
      ELSE
       DIRNAME=TRIM(RESDIR)//'\'//TRIM(ADJUSTL(CTREE(1)))//'\'//TRIM(ADJUSTL(CTREE(2)))//'\'//TRIM(ADJUSTL(CTREE(3)))
!WRITE(*,*) TRIM(DIRNAME)
      ENDIF
     ENDIF
     IF(.NOT.IOSDIREXISTS(TRIM(DIRNAME)))CALL UTL_CREATEDIR(DIRNAME)
     !## store results
     IDFNAME=TRIM(DIRNAME)//'\'//TRIM(PER(IPER)%NAMEPER)//'_'//TRIM(RES(IRES)%NAMERES)//'.idf'

!WRITE(*,*) TRIM(IDFNAME)

!     IDFNAME=TRIM(DIRNAME)//'\'//TRIM(PER(IPER)%NAMEPER)//'_'//TRIM(RES(IRES)%DIRRES)//'_L'//TRIM(ITOS(RES(IRES)%ILAYRES))//'.idf'

     !## write result
     IF(.NOT.IDFWRITE(EFFECT(1),IDFNAME,1))RETURN

     !## only in quickview mode
     IF(LQUICK.AND..NOT.LINVIR)THEN
      !## plot idf in imod
      CALL IDFINIT(IDFNAMEGIVEN=IDFNAME,LEGNAME=TRIM(RES(IRES)%LEGRES),LPLOT=.TRUE.)
      !## draw legend
      CALL LEGPLOT_MAIN(ID_DIR_PMTAB2TAB3,IDF_PICTURE1,3)  !## two columns
     ENDIF

!   IDFNAME=TRIM(DIRNAME)//'\'//'result_equi.idf'
!   IF(.NOT.IDFWRITE_EQUI(EFFECT(1),IDFNAME,RES(J)%ITYPERES))RETURN
!   CALL IDFINIT(IDFNAMEGIVEN=IDFNAME,'')

     !## start computing percentiles of current effect computation
     IF(LINVIR)THEN
      IF(.NOT.IR2LINEAR_PERCENTILES(JFIELD,IIR,JPOL,JPER,JRES,IPER,IRES))RETURN
     ENDIF

    ENDIF
   END DO  ! DO IRES=1,NRES
  ENDIF
 END DO ! DO IPER=1,NPER

 CALL IR2DEALLOCATE()

 IR2COMPUTEIRMAIN=.TRUE.

 END FUNCTION IR2COMPUTEIRMAIN

 !###======================================================================
 FUNCTION IR2CREATEIDF(NIDF)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NIDF
 LOGICAL :: IR2CREATEIDF
 INTEGER :: I,IIDF,NCOL,NROW,TROW,TCOL,NC,NR
 REAL,ALLOCATABLE,DIMENSION(:) :: SX,SY

 IR2CREATEIDF=.FALSE.

 IF(NIDF.LE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'There is no MEASURE that is within the current selection of polygons',&
        'Warning')
  RETURN
 ENDIF

 NCOL=0
 NROW=0
 DO IIDF=1,NIDF
  IF(IDF(IIDF)%IDF%IU.GT.0)THEN
   NCOL=NCOL+IDF(IIDF)%IDF%NCOL+1 !sx(0)
   NROW=NROW+IDF(IIDF)%IDF%NROW+1 !sy(0)
  ENDIF
 END DO

 ALLOCATE(SX(NCOL),SY(NROW))

 TROW=0
 TCOL=0
 DO IIDF=1,NIDF
  IF(IDF(IIDF)%IDF%IU.GT.0)THEN
   DO I=0,IDF(IIDF)%IDF%NCOL
    TCOL    =TCOL+1
    SX(TCOL)=IDF(IIDF)%IDF%SX(I)
   END DO
   DO I=0,IDF(IIDF)%IDF%NROW
    TROW    =TROW+1
    SY(TROW)=IDF(IIDF)%IDF%SY(I)
   END DO
  ENDIF
 ENDDO

!WRITE(*,*) tcol,ncol
!WRITE(*,*) trow,nrow

 CALL UTL_GETUNIQUE(SX,NCOL,NC)
 CALL UTL_GETUNIQUE(SY,NROW,NR)

!WRITE(*,*) 'SORTED'

 IF(ALLOCATED(EFFECT))THEN
  DO I=1,SIZE(EFFECT)
   CALL IDFDEALLOCATEX(EFFECT(I))
  END DO
  DEALLOCATE(EFFECT)
 ENDIF
 ALLOCATE(EFFECT(1))

 NC=NC-1
 NR=NR-1

 EFFECT(1)%NCOL=NC
 EFFECT(1)%NROW=NR
 EFFECT(1)%IEQ =1

 IF(.NOT.IDFALLOCATESXY(EFFECT(1)))RETURN  !allocate idf%sx,idf%sy
 IF(.NOT.IDFALLOCATEX(EFFECT(1)))RETURN    !allocate idf%x

! DO I=2,NC
!  WRITE(*,*) I,NC,SX(I),SX(I)-SX(I-1)
! END DO
! DO I=2,NR
!  WRITE(*,*) I,NR,SY(I),SY(I)-SY(I-1)
! END DO

 DO I=0,NC
  EFFECT(1)%SX(I)=SX(I+1)
 END DO
 DO I=0,NR
  EFFECT(1)%SY(I)=SY(NR-I+1)
 END DO

! DO I=1,NC
!  WRITE(*,*) I,EFFECT(1)%SX(I),EFFECT(1)%SX(I)-EFFECT(1)%SX(I-1)
! END DO
! DO I=1,NR
!  WRITE(*,*) I-1,EFFECT(1)%SY(I),EFFECT(1)%Sy(I-1)-EFFECT(1)%Sy(I)
! END DO

 EFFECT(1)%DX    =0.0
 EFFECT(1)%DY    =0.0
 EFFECT(1)%XMIN  =EFFECT(1)%SX(0)
 EFFECT(1)%XMAX  =EFFECT(1)%SX(NC)
 EFFECT(1)%YMIN  =EFFECT(1)%SY(NR)
 EFFECT(1)%YMAX  =EFFECT(1)%SY(0)
 EFFECT(1)%NODATA=-999.99

 !## find number of unique
 DEALLOCATE(SX,SY)

! CALL UTL_IDFSNAPTOGRID(XMIN,XMAX,YMIN,YMAX,DX,NCOL,NROW)

 IR2CREATEIDF=.TRUE.

 END FUNCTION IR2CREATEIDF

 !###======================================================================
 FUNCTION IR2GETEXTENSION(IC,IFIELD,XMIN,YMIN,XMAX,YMAX,MXCRD)
 !###======================================================================
 IMPLICIT NONE
 LOGICAL :: IR2GETEXTENSION
 INTEGER,INTENT(IN) :: IFIELD,IC
 INTEGER,INTENT(OUT) :: MXCRD
 REAL,INTENT(OUT) :: XMIN,YMIN,XMAX,YMAX
 INTEGER :: IPOL,NCRD
 LOGICAL :: LEX

 IR2GETEXTENSION=.FALSE.

 XMIN = 10.0E10
 YMIN = 10.0E10
 XMAX =-10.0E10
 YMAX =-10.0E10
 MXCRD=0

 IF(IC.EQ.1)THEN
  DO IPOL=1,TTREE(IFIELD)%NPOL
   LEX=.TRUE.
   IF(TTREE(IFIELD)%POL(IPOL)%IACT.NE.1)LEX=.FALSE.
   IF(LEX)THEN
    NCRD =TTREE(IFIELD)%POL(IPOL)%NCRD
    XMIN =MIN(XMIN,MINVAL(TTREE(IFIELD)%POL(IPOL)%X(1:NCRD)))
    XMAX =MAX(XMAX,MAXVAL(TTREE(IFIELD)%POL(IPOL)%X(1:NCRD)))
    YMIN =MIN(YMIN,MINVAL(TTREE(IFIELD)%POL(IPOL)%Y(1:NCRD)))
    YMAX =MAX(YMAX,MAXVAL(TTREE(IFIELD)%POL(IPOL)%Y(1:NCRD)))
    MXCRD=MAX(MXCRD,NCRD)
   ENDIF
  END DO
 ELSEIF(IC.EQ.2)THEN
  DO IPOL=1,MTREE(IFIELD)%NPOL
   LEX=.TRUE.
   IF(MTREE(IFIELD)%POL(IPOL)%IACT.NE.1)LEX=.FALSE.
   IF(LEX)THEN
    NCRD =MTREE(IFIELD)%POL(IPOL)%NCRD
    XMIN =MIN(XMIN,MINVAL(MTREE(IFIELD)%POL(IPOL)%X(1:NCRD)))
    XMAX =MAX(XMAX,MAXVAL(MTREE(IFIELD)%POL(IPOL)%X(1:NCRD)))
    YMIN =MIN(YMIN,MINVAL(MTREE(IFIELD)%POL(IPOL)%Y(1:NCRD)))
    YMAX =MAX(YMAX,MAXVAL(MTREE(IFIELD)%POL(IPOL)%Y(1:NCRD)))
    MXCRD=MAX(MXCRD,NCRD)
   ENDIF
  END DO
 ENDIF

 IF(XMAX.LE.XMIN.OR.YMAX.LE.YMIN)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'There is no polygon given for which the Quick Scan will be performed!', &
        'Warning')
  RETURN
 ENDIF

 IR2GETEXTENSION=.TRUE.

 END FUNCTION IR2GETEXTENSION

 !###======================================================================
 SUBROUTINE IR21DEALLOCATE()
 !###======================================================================
 IMPLICIT NONE

 IF(ALLOCATED(IPR))DEALLOCATE(IPR)
 IF(ALLOCATED(IRS))DEALLOCATE(IRS)
 IF(ALLOCATED(IACT))DEALLOCATE(IACT)
 IF(ALLOCATED(COEF))DEALLOCATE(COEF)

 END SUBROUTINE IR21DEALLOCATE

 !###======================================================================
 SUBROUTINE IR2DEALLOCATE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 IF(ALLOCATED(IDFP))THEN
  DO I=1,SIZE(IDFP)
   CALL IDFDEALLOCATEX(IDFP(I)%IDF)
   IF(IDFP(I)%IDF%IU.GT.0)CLOSE(IDFP(I)%IDF%IU)
   IDFP(I)%IDF%IU=0
  END DO
  DEALLOCATE(IDFP)
 ENDIF
 IF(ALLOCATED(IDF))THEN
  DO I=1,SIZE(IDF)
   CALL IDFDEALLOCATEX(IDF(I)%IDF)
  ENDDO
  DEALLOCATE(IDF)
 ENDIF
 IF(ALLOCATED(EFFECT))THEN
  DO I=1,SIZE(EFFECT)
   CALL IDFDEALLOCATEX(EFFECT(I))
  END DO
  DEALLOCATE(EFFECT)
 ENDIF

 CALL UTL_MESSAGEHANDLE(1)

 END SUBROUTINE IR2DEALLOCATE

 !###======================================================================
 FUNCTION IR2READIDFP(IFIELD,XMIN,YMIN,XMAX,YMAX)
 !###======================================================================
 IMPLICIT NONE
 LOGICAL :: IR2READIDFP
 INTEGER,INTENT(IN) :: IFIELD
 REAL,INTENT(IN) :: XMIN,YMIN,XMAX,YMAX
 INTEGER :: IIR,NC,NR,MXIDF,IOS,TIR
 REAL :: DX,DY

 IR2READIDFP=.FALSE.

 ALLOCATE(IDFP(NIR))

 DX=XMAX-XMIN
 DY=YMAX-YMIN
 NR=0
 NC=0

 !## get/open idf of pointer from IR to estimate max. idf's to be consulted
 DO IIR=1,NIR

  IF(IDFREAD(IDFP(IIR)%IDF,IR(IIR)%IDFIR,0))THEN
   IF(IDFP(IIR)%IDF%IEQ.EQ.1)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Only Equi-IDF"s sustained for referencing!','Error')
    RETURN
   ENDIF
   !## quess number of idf's neccessary
   NC=MAX(NC,INT(DX/REAL(IDFP(IIR)%IDF%DX))+1)
   NR=MAX(NR,INT(DY/REAL(IDFP(IIR)%IDF%DY))+1)
  !## open/read reference table
  ENDIF
 ENDDO

!11414 d11\11414\ir_effect
!11415 d11\11415\ir_effect
!11416 d11\11416\ir_effect

! WRITE(*,*) nc,nr

 IF(.NOT.IR2COUNTIMP(IFIELD,TIR))RETURN

 !## estimated numbers of idf neccessary
 MXIDF=TIR*NC*NR

! WRITE(*,*) 'nc,nr,tir,MXIDF',NC,NR,TIR,MXIDF

 ALLOCATE(IDF(MXIDF),STAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not store current number of IDF"s ['//TRIM(ITOS(MXIDF))//']','Error')
  RETURN
 ENDIF

 IR2READIDFP=.TRUE.

 END FUNCTION IR2READIDFP

 !###======================================================================
 LOGICAL FUNCTION IR2COUNTIMP(IFIELD,TIR)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IFIELD
 INTEGER,INTENT(OUT) :: TIR  !## total impulses
 INTEGER :: IIR,IPOL
 LOGICAL :: LEX

 IR2COUNTIMP=.FALSE.

 !## which ir's are activated ... anyhow!
 TIR=0
 DO IPOL=1,MTREE(IFIELD)%NPOL
  LEX=.TRUE.
  IF(MTREE(IFIELD)%POL(IPOL)%IACT.NE.1)LEX=.FALSE.
  IF(LEX)THEN
   DO IIR=1,MTREE(IFIELD)%POL(IPOL)%NMES
    IF(MTREE(IFIELD)%POL(IPOL)%MES(IIR)%IMP.NE.0.0)TIR=TIR+1
   ENDDO
  ENDIF
 ENDDO

 IF(TIR.EQ.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'There are no active impulses within the current polygon','Error')
  RETURN
 ENDIF

 IR2COUNTIMP=.TRUE.

 END FUNCTION IR2COUNTIMP

 !###======================================================================
 FUNCTION IR2READIDF(MXCRD,IFIELD,XMIN,YMIN,XMAX,YMAX,LQUICK,NIDF,IDFNAME)
 !###======================================================================
 IMPLICIT NONE
 LOGICAL :: IR2READIDF
 CHARACTER(LEN=*),INTENT(IN) :: IDFNAME
 LOGICAL,INTENT(IN) :: LQUICK
 INTEGER,INTENT(OUT) :: NIDF
 INTEGER,INTENT(IN) :: MXCRD,IFIELD
 REAL,INTENT(IN) :: XMIN,YMIN,XMAX,YMAX
 INTEGER :: IPOL,IMES
 LOGICAL :: LEX

 IR2READIDF=.FALSE.

 NIDF=0

 DO IPOL=1,MTREE(IFIELD)%NPOL
  LEX=.TRUE.
  IF(LQUICK)THEN
   IF(MTREE(IFIELD)%POL(IPOL)%IACT.NE.1)LEX=.FALSE.
  ENDIF
  IF(LEX)THEN
   DO IMES=1,MTREE(IFIELD)%POL(IPOL)%NMES
    !## open and read all idf's within current polygon
    IF(.NOT.IR2OPENIRIDF(IMES,NIDF,IPOL,IFIELD,XMIN,YMIN,XMAX,YMAX,IDFNAME))RETURN
   ENDDO
  ENDIF
 ENDDO

 IR2READIDF=.TRUE.

 END FUNCTION IR2READIDF

 !###======================================================================
 FUNCTION IR2OPENIRIDF(IMES,NIDF,IPOL,IFIELD,XMIN,YMIN,XMAX,YMAX,FNAME)
 !###======================================================================
 IMPLICIT NONE
 LOGICAL :: IR2OPENIRIDF
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER,INTENT(IN) :: IPOL,IFIELD,IMES
 INTEGER,INTENT(INOUT) :: NIDF
 REAL,INTENT(IN) :: XMIN,YMIN,XMAX,YMAX
 INTEGER :: I,ID,IROW,ICOL,IC1,IC2,IR1,IR2,IOS,IIR
 REAL :: XC,YC
 CHARACTER(LEN=256) :: IDFNAME
 CHARACTER(LEN=6) :: CID

 IR2OPENIRIDF=.FALSE.

 IF(MTREE(IFIELD)%POL(IPOL)%MES(IMES)%IMP.EQ.0.0)THEN
  IR2OPENIRIDF=.TRUE.
  RETURN
 ENDIF

 !## current ir-impulse
 IIR=MTREE(IFIELD)%POL(IPOL)%MES(IMES)%IMES

 CALL IDFIROWICOL(IDFP(IIR)%IDF,IR2,IC1,XMIN,YMIN)
 CALL IDFIROWICOL(IDFP(IIR)%IDF,IR1,IC2,XMAX,YMAX)

 DO IROW=MAX(1,IR1),MIN(IR2,IDFP(IIR)%IDF%NROW)
  DO ICOL=MAX(1,IC1),MIN(IC2,IDFP(IIR)%IDF%NCOL)

   !## get x/y coordinates
   CALL IDFGETLOC(IDFP(IIR)%IDF,IROW,ICOL,XC,YC)
   IF(UTL_INSIDEPOLYGON(XC,YC,MTREE(IFIELD)%POL(IPOL)%X,MTREE(IFIELD)%POL(IPOL)%Y,MTREE(IFIELD)%POL(IPOL)%NCRD).EQ.1)THEN

    ID=INT(IDFGETVAL(IDFP(IIR)%IDF,IROW,ICOL))

    !## find appropriate id within polygon
    IF(ID.NE.IDFP(IIR)%IDF%NODATA.AND.ID.NE.0)THEN
     WRITE(CID,'(I6.6)') ID
     I=1
     IF(CID(1:1).EQ.'0')I=2
     IDFNAME=TRIM(IR(IIR)%DIRIR)//'\d'//CID(I:3)//'\'//TRIM(ITOS(ID))//'\ir_effect\'//TRIM(FNAME)
     NIDF   =NIDF+1

     IDF(NIDF)%IDF%IU=UTL_GETUNIT()
     !## opening transparant but do not read entire file ... not supported in idfopen()
     CALL OSD_OPEN(IDF(NIDF)%IDF%IU,FILE=IDFNAME,STATUS='OLD',FORM='UNFORMATTED',ACCESS='TRANSPARENT', &
                   ACTION='READ,DENYWRITE',IOSTAT=IOS)
     IF(IOS.NE.0)IDF(NIDF)%IDF%IU=0
     IF(IOS.EQ.0.AND.IDFREADDIM(1,IDF(NIDF)%IDF))THEN
      IDF(NIDF)%IMP=IR1IMPULSEFACTOR(MTREE(IFIELD)%POL(IPOL)%MES(IMES)%IMP,IIR)
     ELSE
      !## not available
      NIDF=NIDF-1
     ENDIF

    ENDIF
   ENDIF

  END DO
 END DO

 IR2OPENIRIDF=.TRUE.

 END FUNCTION IR2OPENIRIDF

 !###======================================================================
 SUBROUTINE IR2COMPUTEIR(NIDF,ITYPERES)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NIDF,ITYPERES
! TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: IIDF,IROW,ICOL,IRAT,IRAT1!,ITYPE
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IPCOL,IPROW
 REAL :: XY,DX,DY

 CALL WINDOWSELECT(0)

 IF(ALLOCATED(IPCOL))DEALLOCATE(IPCOL)
 IF(ALLOCATED(IPROW))DEALLOCATE(IPROW)

 EFFECT(1)%X=0.0
 IRAT1      =0

 DO IIDF=1,NIDF
!  CALL WMESSAGEPEEK(ITYPE,MESSAGE)
  IF(IDF(IIDF)%IDF%IU.GT.0)THEN

   IF(ALLOCATED(IPCOL))THEN
    IF(SIZE(IPCOL).LT.IDF(IIDF)%IDF%NCOL+1)DEALLOCATE(IPCOL)
   ENDIF
   IF(ALLOCATED(IPROW))THEN
    IF(SIZE(IPROW).LT.IDF(IIDF)%IDF%NROW+1)DEALLOCATE(IPROW)
   ENDIF
   IF(.NOT.ALLOCATED(IPCOL))ALLOCATE(IPCOL(0:IDF(IIDF)%IDF%NCOL))
   IF(.NOT.ALLOCATED(IPROW))ALLOCATE(IPROW(0:IDF(IIDF)%IDF%NROW))

   CALL IR2GETPOINTERS(IIDF,IPCOL,SIZE(IPCOL),IPROW,SIZE(IPROW))

!WRITE(*,*) 'IMP=',IIDF,IDF(IIDF)%IMP!,ITYPERES,IDF(IIDF)%IDF%IEQ

   !## m3/d and non-equidistantial raster
   IF(ITYPERES.EQ.1.AND.IDF(IIDF)%IDF%IEQ.EQ.1)THEN

    DO IROW=1,IDF(IIDF)%IDF%NROW
     DY=IDF(IIDF)%IDF%SY(IROW-1)-IDF(IIDF)%IDF%SY(IROW)
     DO ICOL=1,IDF(IIDF)%IDF%NCOL

      !## read idfvalue
      READ(IDF(IIDF)%IDF%IU) XY

      !## include impulse strength
      XY=XY*IDF(IIDF)%IMP

      !## transform from m3/d -> mm/d
      DX=IDF(IIDF)%IDF%SX(ICOL)-IDF(IIDF)%IDF%SX(ICOL-1)
      XY=1000.0*XY/(DX*DY)

      CALL IR2ASSIGNEFFECT(IROW,ICOL,IPCOL,SIZE(IPCOL),IPROW,SIZE(IPROW),XY)

     END DO
    END DO

   !## m or m3/d on equidistantial raster
   ELSE

    DO IROW=1,IDF(IIDF)%IDF%NROW
     DO ICOL=1,IDF(IIDF)%IDF%NCOL

      !## read idfvalue
      READ(IDF(IIDF)%IDF%IU) XY

      !## include impulse strength
      XY=XY*IDF(IIDF)%IMP

      CALL IR2ASSIGNEFFECT(IROW,ICOL,IPCOL,SIZE(IPCOL),IPROW,SIZE(IPROW),XY)

     END DO
    END DO

   ENDIF

   CLOSE(IDF(IIDF)%IDF%IU)
   IDF(IIDF)%IDF%IU=0

  ENDIF
  CALL UTL_WAITMESSAGE(IRAT,IRAT1,IIDF,NIDF,'Computing IR ...')
 END DO

 !## correct in case of m/d -> m3/dag
 IF(ITYPERES.EQ.1)THEN
  !## translate m3/d -> mm/d
  IF(EFFECT(1)%IEQ.EQ.0)THEN
   DO IROW=1,EFFECT(1)%NROW
    DY=EFFECT(1)%SY(IROW-1)-EFFECT(1)%SY(IROW)
    DO ICOL=1,EFFECT(1)%NCOL
     DX=EFFECT(1)%SX(ICOL)-EFFECT(1)%SX(ICOL-1)
     EFFECT(1)%X(ICOL,IROW)=1000.0*EFFECT(1)%X(ICOL,IROW)/(DX*DY)
    ENDDO
   ENDDO
  ENDIF
 ENDIF

 IF(ALLOCATED(IPCOL))DEALLOCATE(IPCOL)
 IF(ALLOCATED(IPROW))DEALLOCATE(IPROW)

 END SUBROUTINE IR2COMPUTEIR

 !###======================================================================
 SUBROUTINE IR2ASSIGNEFFECT(IROW,ICOL,IPCOL,SIPCOL,IPROW,SIPROW,XY)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: SIPCOL,SIPROW,IROW,ICOL
 INTEGER,DIMENSION(0:SIPCOL),INTENT(IN) :: IPCOL
 INTEGER,DIMENSION(0:SIPROW),INTENT(IN) :: IPROW
 REAL,INTENT(IN) :: XY
 INTEGER :: IR,IC

 !## put result into proper location in effect%idf%x(:,:)
 DO IR=IPROW(IROW-1)+1,IPROW(IROW)
  DO IC=IPCOL(ICOL-1)+1,IPCOL(ICOL)
   EFFECT(1)%X(IC,IR)=EFFECT(1)%X(IC,IR)+XY
  END DO
 END DO

 END SUBROUTINE IR2ASSIGNEFFECT

 !###======================================================================
 SUBROUTINE IR2GETPOINTERS(IIDF,IPCOL,NIPCOL,IPROW,NIPROW)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IIDF,NIPCOL,NIPROW
 INTEGER,DIMENSION(0:NIPCOL),INTENT(OUT) :: IPCOL
 INTEGER,DIMENSION(0:NIPROW),INTENT(OUT) :: IPROW
 INTEGER :: I,J

 J=0
 DO I=0,IDF(IIDF)%IDF%NCOL
  DO
   IF(EFFECT(1)%SX(J).GE.IDF(IIDF)%IDF%SX(I))EXIT
   J=J+1
  END DO
  IPCOL(I)=J
  J     =J+1
 END DO
 J=0
 DO I=0,IDF(IIDF)%IDF%NROW
  DO
   IF(EFFECT(1)%SY(J).LE.IDF(IIDF)%IDF%SY(I))EXIT
   J=J+1
  END DO
  IPROW(I)=J
  J     =J+1
 END DO

 END SUBROUTINE IR2GETPOINTERS

END MODULE MOD_IR_CLC
