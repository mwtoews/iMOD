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
MODULE MOD_MATH_CALC

 USE WINTERACTER
 USE RESOURCE
 USE MOD_IDF_PAR, ONLY : IDFOBJ
 USE MOD_IDF, ONLY : IDFOPEN,IDFREAD,IDFWRITE,IDFDEALLOCATE,IDFALLOCATEX,IDFALLOCATESXY,IDFGETVAL,IDFWRITE_EQUI,IDFGETLOC,&
                     IDFIROWICOL,IDFGETVAL_CHECK,IDFFILLCOMMENT,IDFNULLIFY,IDFPUTVAL,IDFWRITEDIM,IDFCOPY,IDFGETILAY,IDFEQUAL,&
                     IDFREADSCALE
 USE MOD_UTL, ONLY : UTL_GETUNIT,UTL_CHECKNAME,UTL_CREATEDIR,UTL_IDFSNAPTOGRID,UTL_MESSAGEHANDLE,UTL_WAITMESSAGE,ITOS,RTOS, &
        NEWLINE,UTL_INSIDEPOLYGON,UTL_IDFSNAPTOGRID_LLC,UTL_DIRINFO_POINTER,UTL_IDATETOJDATE,UTL_IDFGETDATE,UTL_CAP,UTL_SUBST
 USE MOD_POLINT, ONLY : POL1LOCATE 
 USE MOD_POLYGON_UTL, ONLY : POLYGON1CLOSE,POLYGON1INIT,POLYGON1SAVELOADSHAPE
 USE MOD_POLYGON_PAR
 USE MOD_OSD, ONLY : OSD_OPEN 
 USE MODPLOT
 USE MOD_MATH_PAR
 USE MOD_PREF_PAR, ONLY : PREFVAL
 USE MOD_GXG_CLC, ONLY : GXG1GETGT,GXG1GTLEG_WRITE
 USE MOD_QKSORT

 TYPE(IDFOBJ),DIMENSION(:),ALLOCATABLE :: MATH
 REAL,ALLOCATABLE,DIMENSION(:) :: DELR,DELC
 INTEGER,ALLOCATABLE,DIMENSION(:,:) :: PDELR,PDELC

CONTAINS

 !###======================================================================
 LOGICAL FUNCTION MATH1CALC(IBATCH,LEGNAME)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH
 CHARACTER(LEN=*),INTENT(OUT) :: LEGNAME
 INTEGER :: IC1,IC2,IR1,IR2,IREPEAT
 INTEGER,DIMENSION(3) :: IOP,IG
 REAL :: CLC(3)
 LOGICAL :: LEX
 CHARACTER(LEN=80) :: STRING 
 CHARACTER(LEN=256) :: AS,BS
 INTEGER :: CIDF,I,IEFUNC

 MATH1CALC=.FALSE.

 IF(IGEN.EQ.1)THEN
  INQUIRE(FILE=GENNAME,EXIST=LEX)
  IF(.NOT.LEX)THEN
   IF(TRIM(GENNAME).EQ.'')THEN
    IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'No *.gen given!','Warning')
    IF(IBATCH.EQ.1)WRITE(*,*) 'No *.gen given!'
   ELSE
    IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Genfile: '//TRIM(GENNAME)//CHAR(13)// &
    'does not exists!','Warning')
    IF(IBATCH.EQ.1)WRITE(*,*) 'Genfile: '//TRIM(GENNAME)//' does not exists!'
   ENDIF
   RETURN
  ENDIF
  IF(IBATCH.EQ.0)CALL POLYGON1INIT()
  CALL POLYGON1SAVELOADSHAPE(ID_LOADSHAPE,0,GENNAME)
 ENDIF

 IF(.NOT.MATH1GETFUNC(FUNC,IBATCH,IG,IOP,CLC,IEFUNC))RETURN

 ALLOCATE(MATH(3)); DO I=1,3; CALL IDFNULLIFY(MATH(I)); ENDDO
 
 DO IREPEAT=1,SIZE(IDFNAMES,1)
 
  !## get new idfnames
  IF(IG(3).GT.0)THEN
   CALL UTL_CHECKNAME(IDFNAMES(IREPEAT,3),'IDF')
   I=INDEXNOCASE(IDFNAMES(IREPEAT,3),'\',.TRUE.)-1
   CALL UTL_CREATEDIR(IDFNAMES(IREPEAT,3)(:I))
  ENDIF

  DO I=1,2
   IF(IG(I).GT.0)THEN
    INQUIRE(FILE=IDFNAMES(IREPEAT,I),EXIST=LEX)
    IF(.NOT.LEX)THEN
     IF(IBATCH.EQ.0)THEN
      IF(TRIM(IDFNAMES(IREPEAT,I)).EQ.'')THEN
       CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'No input map is given!','Warning')
      ELSE
       CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Input Map: ['//TRIM(IDFNAMES(IREPEAT,I))//']'//CHAR(13)// &
       'does not exists!','Warning')
      ENDIF
      RETURN
     ENDIF
     IF(IBATCH.EQ.1)THEN
      IF(TRIM(IDFNAMES(IREPEAT,I)).EQ.'')THEN
       WRITE(*,'(A)') 'No input map is given!'
      ELSE
       WRITE(*,'(A)') 'Input Map: ['//TRIM(IDFNAMES(IREPEAT,I))//']'//' does not exists!'
      ENDIF
      EXIT
     ENDIF
    ENDIF
   ENDIF
  ENDDO

  !## take the next combination of idf files
  IF(I.GT.2)THEN
 
   IF(IBATCH.EQ.0)THEN
    INQUIRE(FILE=IDFNAMES(IREPEAT,3),EXIST=LEX)
    IF(LEX)THEN
     CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Output Map: '//TRIM(IDFNAMES(IREPEAT,3))//CHAR(13)// &
     'already exists, overwrite it (choose yes) or cancel (choose no) ?','Question')
     IF(WINFODIALOG(4).NE.1)RETURN
    ENDIF
   ENDIF

   IF(IBATCH.EQ.0)THEN
    CALL WINDOWSELECT(0); CALL WINDOWOUTSTATUSBAR(4,'Press Escape to stop operation: Progress 0%')
    CALL WMESSAGEENABLE(KEYDOWN,1)
   ELSE
    WRITE(*,'(A,I3)') 'Busy computing sequence ',IREPEAT
   ENDIF

   !## first names is cidf-file
   MATH(1)%IU=0
   MATH(2)%IU=0
   IF(IG(1).GT.0.AND.IG(2).EQ.0)THEN
    CIDF=1  !C=A
    IF(.NOT.IDFREAD(MATH(1),IDFNAMES(IREPEAT,1),0))RETURN
   ELSEIF(IG(1).EQ.0.AND.IG(2).GT.0)THEN
    CIDF=2  !C=B
    IF(.NOT.IDFREAD(MATH(1),IDFNAMES(IREPEAT,2),0))RETURN
   ELSE
    IF(IG(1).LT.IG(2))THEN
     CIDF=1 !A
     IF(.NOT.IDFREAD(MATH(1),IDFNAMES(IREPEAT,1),0))RETURN
     IF(.NOT.IDFREAD(MATH(2),IDFNAMES(IREPEAT,2),0))RETURN
    ELSEIF(IG(2).LT.IG(1))THEN
     CIDF=1 !B
     IF(.NOT.IDFREAD(MATH(1),IDFNAMES(IREPEAT,2),0))RETURN
     IF(.NOT.IDFREAD(MATH(2),IDFNAMES(IREPEAT,1),0))RETURN
    ENDIF
   ENDIF

   !## determine size/parameters of result idf
   CALL IDFCOPY(MATH(CIDF),MATH(3))

   !## overrule nodata value for fluxdiff function
   IF(IEFUNC.EQ.5)MATH(3)%NODATA=-999.00

   !## first named selected idf's
   IF(IIEXT.EQ.2)THEN
    MATH(3)%NCOL=MATH(CIDF)%NCOL
    MATH(3)%NROW=MATH(CIDF)%NROW
    IC1=1; IC2=MATH(CIDF)%NCOL
    IR1=1; IR2=MATH(CIDF)%NROW
   !## current zoom window
   ELSEIF(IIEXT.EQ.1)THEN
    !## check whether (part of) map within current zoom-window
    IF((MPW%XMIN.GT.MATH(3)%XMAX.OR.MPW%XMAX.LT.MATH(3)%XMIN.OR.  &
        MPW%YMIN.GT.MATH(3)%YMAX.OR.MPW%YMAX.LT.MATH(3)%YMIN))THEN
     IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Selected Maps outside current window','Error: Termination')
     IF(IBATCH.EQ.1)WRITE(*,*) 'Selected Maps outside current window'
     RETURN
    ENDIF
    IF(MATH(3)%IEQ.EQ.0)THEN

     I   =(MPW%XMIN-MATH(CIDF)%XMIN)/MATH(CIDF)%DX
     MATH(3)%XMIN =MATH(CIDF)%XMIN+I*MATH(CIDF)%DX
     I   =(MPW%XMAX-MATH(CIDF)%XMIN)/MATH(CIDF)%DX
     MATH(3)%XMAX =MATH(CIDF)%XMIN+I*MATH(CIDF)%DX
     I   =(MATH(CIDF)%YMAX-MPW%YMIN)/MATH(CIDF)%DY
     MATH(3)%YMIN =MATH(CIDF)%YMAX-I*MATH(CIDF)%DY
     I   =(MATH(CIDF)%YMAX-MPW%YMAX)/MATH(CIDF)%DY
     MATH(3)%YMAX =MATH(CIDF)%YMAX-I*MATH(CIDF)%DY
     !#not bigger than selected idf's
     DO I=1,2
      IF(IG(I).NE.0)THEN
       MATH(3)%XMIN=MAX(MATH(I)%XMIN,MATH(3)%XMIN)
       MATH(3)%XMAX=MIN(MATH(I)%XMAX,MATH(3)%XMAX)
       MATH(3)%YMIN=MAX(MATH(I)%YMIN,MATH(3)%YMIN)
       MATH(3)%YMAX=MIN(MATH(I)%YMAX,MATH(3)%YMAX)
      ENDIF
     ENDDO
     CALL UTL_IDFSNAPTOGRID(MATH(3)%XMIN,MATH(3)%XMAX,MATH(3)%YMIN,MATH(3)%YMAX,MATH(3)%DX,MATH(3)%NCOL,MATH(3)%NROW)
 
    ELSEIF(MATH(3)%IEQ.EQ.1)THEN

     !## make sure idf is within window
     CALL POL1LOCATE(MATH(CIDF)%SX,MATH(CIDF)%NCOL+1,REAL(MPW%XMIN,8),IC1)
     IC1=MAX(1,IC1)
     CALL POL1LOCATE(MATH(CIDF)%SX,MATH(CIDF)%NCOL+1,REAL(MPW%XMAX,8),IC2)
     IC2=MIN(IC2,MATH(CIDF)%NCOL)
     CALL POL1LOCATE(MATH(CIDF)%SY,MATH(CIDF)%NROW+1,REAL(MPW%YMIN,8),IR2)
     IR2=MIN(IR2,MATH(CIDF)%NROW)
     CALL POL1LOCATE(MATH(CIDF)%SY,MATH(CIDF)%NROW+1,REAL(MPW%YMAX,8),IR1)
     IR1=MAX(1,IR1)

     MATH(3)%XMIN=MATH(CIDF)%SX(IC1-1)!)
     MATH(3)%XMAX=MATH(CIDF)%SX(IC2)
     MATH(3)%YMIN=MATH(CIDF)%SY(IR2)
     MATH(3)%YMAX=MATH(CIDF)%SY(IR1-1)!)
     MATH(3)%NCOL=IC2-IC1+1
     MATH(3)%NROW=IR2-IR1+1

    ENDIF

   !## given window
   ELSEIF(IIEXT.EQ.3)THEN

    IF(MATH(3)%IEQ.EQ.1)THEN
     IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot combine IEXT=3 and non-equidistantial IDF"s!','Error')
     IF(IBATCH.EQ.1)WRITE(*,*) 'Cannot combine IEXT=3 and non-equidistantial IDF"s!'
     RETURN
    ENDIF
    MATH(3)%XMIN=MPW%XMIN; MATH(3)%XMAX=MPW%XMAX
    MATH(3)%YMIN=MPW%YMIN; MATH(3)%YMAX=MPW%YMAX
    
    CALL UTL_IDFSNAPTOGRID_LLC(MATH(3)%XMIN,MATH(3)%XMAX,MATH(3)%YMIN,MATH(3)%YMAX,MATH(3)%DX,MATH(3)%NCOL,MATH(3)%NROW)

   ENDIF
 
   IF(.NOT.IDFALLOCATEX(MATH(3)))RETURN

   IF(MATH(3)%IEQ.EQ.1)THEN
    IF(.NOT.IDFALLOCATESXY(MATH(3)))RETURN
    MATH(3)%SX(0:MATH(3)%NCOL)=MATH(CIDF)%SX(IC1-1:IC2)
    MATH(3)%SY(0:MATH(3)%NROW)=MATH(CIDF)%SY(IR1-1:IR2)
   ENDIF

   CALL MATH1COMP(LEX,IOP,CLC,STRING,IEFUNC,IBATCH)

   IF(LEX)THEN
 
    AS='A: none'
    BS='B: none'
    IF(INDEX(FUNC,'A').GT.0)AS=AS(1:3)//TRIM(IDFNAMES(IREPEAT,1)) 
    IF(INDEX(FUNC,'B').GT.0)BS=BS(1:3)//TRIM(IDFNAMES(IREPEAT,2))
    CALL IDFFILLCOMMENT(MATH(3),'Units: Unknown'//NEWLINE// &
                                'Formulae: '//TRIM(FUNC)//NEWLINE// &
                                TRIM(AS)//NEWLINE// &
                                TRIM(BS))
 
    !## result idf is equidistantial
    I=0
    IF(MATH(3)%IEQ.EQ.1)I=IEQUI
    IF(I.EQ.0)LEX=IDFWRITE(MATH(3),IDFNAMES(IREPEAT,3),1)
    IF(I.EQ.1)LEX=IDFWRITE_EQUI(MATH(3),IDFNAMES(IREPEAT,3))!,0)
    IF(.NOT.LEX)THEN
     IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot write IDF '//TRIM(IDFNAMES(IREPEAT,3)),'Error: Termination')
     IF(IBATCH.EQ.1)WRITE(*,*) 'Cannot write IDF '//TRIM(IDFNAMES(IREPEAT,3))
    ENDIF
   ELSE
    IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,STRING,'Error: Termination')
    IF(IBATCH.EQ.1)WRITE(*,*) TRIM(STRING)
   ENDIF
  
  ENDIF

  CALL IDFDEALLOCATE(MATH,SIZE(MATH))

 ENDDO
 
 LEGNAME=''; IF(IEFUNC.EQ.4)LEGNAME=TRIM(PREFVAL(1))//'\tmp\gt.leg'
 MATH1CALC=LEX
 
 END FUNCTION MATH1CALC

 !###======================================================================
 SUBROUTINE MATH1CALCCLOSE(IBATCH) 
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH 

 CALL POLYGON1CLOSE()

 IF(IBATCH.EQ.0)THEN
  CALL WINDOWSELECT(0)
  CALL WINDOWOUTSTATUSBAR(4,'')
 ENDIF

 !## module used outside imod
 IF(ALLOCATED(IDFNAMES))DEALLOCATE(IDFNAMES)
 IF(ALLOCATED(MATH))THEN
  CALL IDFDEALLOCATE(MATH,SIZE(MATH))
  DEALLOCATE(MATH)
 ENDIF

 END SUBROUTINE MATH1CALCCLOSE

 !###======================================================================
 SUBROUTINE MATH1PLOTFUNC(FUNC)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(INOUT) :: FUNC
 INTEGER :: IEFUNC,I
 INTEGER,DIMENSION(3) :: IG,IOP
 REAL,DIMENSION(3) :: CLC

 I=1
 IF(.NOT.MATH1GETFUNC(FUNC,-1,IG,IOP,CLC,IEFUNC))I=0
 CALL WDIALOGSELECT(ID_DMATH)
 CALL WDIALOGFIELDSTATE(IDOK,I)
 CALL WDIALOGFIELDSTATE(IDF_RADIO2,I)
 IF(I.EQ.0)THEN
  CALL WDIALOGPUTSTRING(IDOK,'Incorrect Formulae')
  CALL WDIALOGPUTSTRING(IDF_RADIO2,'Unknown Map') 
 ELSE
  CALL WDIALOGPUTSTRING(IDOK,'Compute ...')
  IF(IG(1).EQ.0.AND.IG(2).GT.0)CALL WDIALOGPUTSTRING(IDF_RADIO2,'Map B') 
  IF(IG(1).GT.0.AND.IG(2).EQ.0)CALL WDIALOGPUTSTRING(IDF_RADIO2,'Map A') 
  IF(IG(1).GT.0.AND.IG(2).GT.0)THEN
   IF(IG(1).LT.IG(2))CALL WDIALOGPUTSTRING(IDF_RADIO2,'Map A') 
   IF(IG(2).LT.IG(1))CALL WDIALOGPUTSTRING(IDF_RADIO2,'Map B') 
  ENDIF
 ENDIF
 
 END SUBROUTINE MATH1PLOTFUNC

 !###======================================================================
 LOGICAL FUNCTION MATH1GETFUNC(FUNC,IBATCH,IG,IOP,CLC,IEFUNC)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH
 INTEGER,INTENT(OUT) :: IEFUNC
 CHARACTER(LEN=*),INTENT(OUT) :: FUNC
 INTEGER,DIMENSION(3),INTENT(OUT) :: IG,IOP
 REAL,INTENT(OUT),DIMENSION(3) :: CLC
 INTEGER,DIMENSION(3) :: IOS
 INTEGER :: IIS,I,J
 CHARACTER(LEN=50) :: TXT
 CHARACTER(LEN=1),DIMENSION(5) :: COP
 CHARACTER(LEN=1),DIMENSION(2) :: CAB
 CHARACTER(LEN=3),DIMENSION(5) :: CEXP
 INTEGER,DIMENSION(2) :: IO
 DATA COP/'-','+','/','*','?'/
 DATA CEXP/'ABS','LOG','EXP','GTP','SGN'/
 DATA CAB/'A','B'/
 CHARACTER(LEN=10),DIMENSION(2) :: TXT_CLC
 
 MATH1GETFUNC=.FALSE.

 !## get function
 FUNC=UTL_CAP(FUNC,'U') 
 IIS=INDEX(FUNC,'C=')
 IF(IIS.EQ.0)THEN
  IF(IBATCH.EQ.-1)CALL WDIALOGPUTSTRING(IDF_LABEL5,'Function not well defined')
  IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Function not defined!'//CHAR(13)// &
        'Function should start with C=','Warning')
  IF(IBATCH.EQ.1)WRITE(*,*) 'Function should start with C='
  RETURN
 ENDIF

 !## determine external function first
 I=INDEX(FUNC,'(')
 IF(I.GT.0)THEN
  J=INDEX(FUNC,')')
  IF(J.LT.I)THEN
   IF(IBATCH.EQ.-1)CALL WDIALOGPUTSTRING(IDF_LABEL5,'External Function not well defined')
   IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'External Function not well defined!'//CHAR(13)// &
     'External Function should start with C=abs() or C=log()','Warning')
   IF(IBATCH.EQ.1)WRITE(*,*) 'External Function should start with C=abs() or C=log()'
   RETURN
  ENDIF
  SELECT CASE (FUNC(3:5))
   CASE ('ABS')
    IEFUNC=1
   CASE ('LOG')
    IEFUNC=2
   CASE ('EXP')
    IEFUNC=3
   CASE ('GTP')
    IEFUNC=4
   CASE ('SGN')
    IEFUNC=5
   CASE DEFAULT
    IF(IBATCH.EQ.-1)CALL WDIALOGPUTSTRING(IDF_LABEL5,'Cannot recognize External Function')
    IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'External Function not well defined!'//CHAR(13)// &
     'External Function can be abs(), log(), exp(), gtp(), sgn()','Warning')
    IF(IBATCH.EQ.1)WRITE(*,*) 'External Function can be abs(), log(), exp(), gtp(), sgn()'
    RETURN
  END SELECT
  FUNC='C='//FUNC(I+1:J-1)
 ELSE
  IEFUNC=0
 ENDIF

 !## determine which idf's are active
 IG(1)=INDEX(FUNC,'A')
 IG(2)=INDEX(FUNC,'B')
 IG(3)=INDEX(FUNC,'C')

 IF(IG(1).EQ.0.AND.IG(2).EQ.0)THEN
  IF(IBATCH.EQ.-1)CALL WDIALOGPUTSTRING(IDF_LABEL5,'Missing at least one file (A or B) in Function')
  IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Missing at least one file (A or B) in Function','Warning')
  IF(IBATCH.EQ.1)WRITE(*,*) 'Missing at least one file (A or B) in Function'
  RETURN
 ENDIF

 CLC=1.0
 IOP=0
 IOS=0

 !## only A is active, B is not!
 IF(IG(2).EQ.0.AND.IG(1).GT.0)THEN
  IOP(1)=MATHGETIOP(FUNC(IG(1)-1:IG(1)-1))     !get operator before A
  IF(IOP(1).GT.0)CLC(1)=MATHGETCLC(FUNC(IG(3)+2:IG(1)-2),IOS(1)) !get x-factor before A
  IF(IOP(1).EQ.0)CLC(1)=1.0
  TXT_CLC(1)=FUNC(IG(3)+2:IG(1)-2)
 !## only B is active, A is not!
 ELSEIF(IG(1).EQ.0.AND.IG(2).GT.0)THEN
  IOP(2)=MATHGETIOP(FUNC(IG(2)-1:IG(2)-1))     !get operator before B
  IF(IOP(2).GT.0)CLC(2)=MATHGETCLC(FUNC(IG(3)+2:IG(2)-2),IOS(2)) !get x-factor before B
  IF(IOP(2).EQ.0)CLC(2)=1.0
  TXT_CLC(2)=FUNC(IG(3)+2:IG(2)-2)
 !## both A and B are active
 ELSE
  IOP(1)=MATHGETIOP(FUNC(IG(1)-1:IG(1)-1))   !get operator before A
  IOP(2)=MATHGETIOP(FUNC(IG(2)-1:IG(2)-1))   !get operator before B
 !## C=AB
  IF(IG(2).GT.IG(1))THEN
   !## no factor used
   IF(IG(1).EQ.3)IOP(1)=0
   IF(IOP(1).NE.0.AND.(IG(3)+2)-(IG(1)-2).LE.0)THEN
    CLC(1)=MATHGETCLC(FUNC(IG(3)+2:IG(1)-2),IOS(1)) !get x-factor before A
    TXT_CLC(1)=FUNC(IG(3)+2:IG(1)-2)
   ELSE
    IOP(1)=0
   ENDIF
   IF(IG(2)-IG(1).LE.2)IOP(2)=0
   IF(IOP(2).NE.0.AND.(IG(1)+2)-(IG(2)-2).LE.0)THEN
    CLC(2)=MATHGETCLC(FUNC(IG(1)+2:IG(2)-2),IOS(2)) !get x-factor before B
    TXT_CLC(2)=FUNC(IG(1)+2:IG(2)-2)
   ELSE
    IOP(2)=0
   ENDIF
   IOP(3)=MATHGETIOP(FUNC(IG(1)+1:IG(1)+1))
  !## C=BA
  ELSE
   !## no factor used
   IF(IG(2).EQ.3)IOP(2)=0
   IF(IOP(1).NE.0.AND.(IG(2)+2)-(IG(1)-2).LE.0)THEN
    CLC(1)=MATHGETCLC(FUNC(IG(2)+2:IG(1)-2),IOS(1)) !get x-factor before A
    TXT_CLC(1)=FUNC(IG(2)+2:IG(1)-2)
   ELSE
    IOP(1)=0
   ENDIF
   IF(IG(1)-IG(2).LE.2)IOP(1)=0
   IF(IOP(2).NE.0.AND.(IG(3)+2)-(IG(2)-2).LE.0)THEN
    CLC(2)=MATHGETCLC(FUNC(IG(3)+2:IG(2)-2),IOS(2)) !get x-factor before B
    TXT_CLC(2)=FUNC(IG(3)+2:IG(2)-2)
   ELSE
    IOP(2)=0
   ENDIF
   IOP(3)=MATHGETIOP(FUNC(IG(2)+1:IG(2)+1))
  ENDIF
  IF(IOP(3).EQ.0)THEN
   IF(IBATCH.EQ.-1)CALL WDIALOGPUTSTRING(IDF_LABEL5,'NO correct mathematical expression given between Map A and Map B')
   IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK, &
       'Not correct mathematical expression given between Map A and Map B'//CHAR(13)// &
       'Expression should consists of a single -,+,/,*,?,<,> sign','Warning')
   IF(IBATCH.EQ.1)WRITE(*,*) 'Not correct mathematical expression given between Map A and Map B. '// &
       'Expression should consists of a single -,+,/,*,?,<,> sign'
   RETURN
  ENDIF
 ENDIF

 IF(IBATCH.EQ.-1)THEN

  TXT='C='
  IF(IEFUNC.NE.0)TXT=TRIM(TXT)//TRIM(CEXP(IEFUNC))//'('
  IO(1)=1
  IO(2)=2
  IF(IG(1).GT.IG(2))THEN
   IO(1)=2
   IO(2)=1
  ENDIF

  IF(IG(IO(1)).NE.0)THEN
   IF(IOP(IO(1)).NE.0)THEN
    TXT=TRIM(TXT)//TRIM(TXT_CLC(IO(1)))//TRIM(COP(IOP(IO(1))))//CAB(IO(1))
   ELSE
    TXT=TRIM(TXT)//CAB(IO(1))
   ENDIF
  ENDIF
  IF(IOP(3).NE.0)TXT=TRIM(TXT)//TRIM(COP(IOP(3)))
  IF(IG(IO(2)).NE.0)THEN
   IF(IOP(IO(2)).NE.0)THEN
    TXT=TRIM(TXT)//TRIM(TXT_CLC(IO(2)))//TRIM(COP(IOP(IO(2))))//CAB(IO(2))
   ELSE
    TXT=TRIM(TXT)//CAB(IO(2))
   ENDIF
  ENDIF
  IF(IEFUNC.NE.0)TXT=TRIM(TXT)//')'
  CALL WDIALOGPUTSTRING(IDF_LABEL5,'Expression= '//TRIM(TXT))
 ENDIF

 MATH1GETFUNC=.TRUE.

 END FUNCTION MATH1GETFUNC

 !###======================================================================
 INTEGER FUNCTION MATHGETIOP(FUNC)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FUNC

 MATHGETIOP=0

 IF(INDEX(FUNC,'-').NE.0)MATHGETIOP=1
 IF(INDEX(FUNC,'+').NE.0)MATHGETIOP=2
 IF(INDEX(FUNC,'/').NE.0)MATHGETIOP=3
 IF(INDEX(FUNC,'*').NE.0)MATHGETIOP=4
 IF(INDEX(FUNC,'?').NE.0)MATHGETIOP=5
 IF(INDEX(FUNC,'<').NE.0)MATHGETIOP=6  !## smallest
 IF(INDEX(FUNC,'>').NE.0)MATHGETIOP=7  !## bigest
 
 END FUNCTION MATHGETIOP

 !###======================================================================
 REAL FUNCTION MATHGETCLC(FUNC,IOS)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FUNC
 INTEGER,INTENT(OUT) :: IOS

 READ(FUNC,*,IOSTAT=IOS) MATHGETCLC
 IF(IOS.NE.0)MATHGETCLC=1.0

 END FUNCTION MATHGETCLC

 !###======================================================================
 SUBROUTINE MATH1COMP(LEX,IOP,CLC,STRING,IEFUNC,IBATCH)
 !###======================================================================
 IMPLICIT NONE
 TYPE (WIN_MESSAGE) :: MESSAGE
 CHARACTER(LEN=*),INTENT(OUT) :: STRING
 INTEGER,INTENT(IN) :: IEFUNC,IBATCH
 INTEGER,INTENT(IN),DIMENSION(3) :: IOP
 REAL,INTENT(IN),DIMENSION(3) :: CLC
 LOGICAL,INTENT(OUT) :: LEX
 INTEGER :: IROW,ICOL,ITYPE,IRAT1,IRAT,I
 REAL :: XC,YC,IDFRESULT
 LOGICAL :: LCOMP

 IF(IBATCH.EQ.0)THEN
  CALL UTL_MESSAGEHANDLE(0)
  CALL WMESSAGEENABLE(KEYDOWN,1)
 ENDIF

 LEX=.FALSE.

 !## start diff.-computation
 STRING='Process terminated by user'

 IRAT =0
 IRAT1=IRAT
 DO IROW=1,MATH(3)%NROW

  IF(IBATCH.EQ.0)THEN
   CALL WMESSAGEPEEK(ITYPE, MESSAGE)
   IF(ITYPE.EQ.KEYDOWN.AND.MESSAGE%VALUE1.EQ.KEYESCAPE)RETURN
  ENDIF

  DO ICOL=1,MATH(3)%NCOL

   CALL IDFGETLOC(MATH(3),IROW,ICOL,XC,YC)

   LCOMP=.TRUE.
   IF(IGEN.EQ.1)THEN
    DO I=1,SHPNO
     IF(UTL_INSIDEPOLYGON(XC,YC,SHPXC(:,I),SHPYC(:,I),SHPNCRD(I)).EQ.1)EXIT
    END DO
    IF(I.GT.SHPNO)LCOMP=.FALSE.
   ENDIF

   CALL MATH1CALCVALUE(XC,YC,IOP,CLC,LCOMP,IDFRESULT,IEFUNC)

   IF(LCOMP)THEN
    IF(IDFRESULT.NE.MATH(3)%NODATA)THEN
     SELECT CASE (IEFUNC)
      CASE (1)  !## abs
       IDFRESULT=ABS(IDFRESULT)
      CASE (2)  !## log
       IF(IDFRESULT.GT.0.0)THEN
        IDFRESULT=LOG(IDFRESULT)
       ENDIF
      CASE (3)  !## exp
       IDFRESULT=EXP(IDFRESULT)
      CASE (4)  !## gt
!       IDFRESULT=EXP(IDFRESULT)
      CASE (5)  !## sgn

     END SELECT
    ENDIF
   ENDIF

   MATH(3)%X(ICOL,IROW)=IDFRESULT

  ENDDO
  IF(IBATCH.EQ.0)CALL UTL_WAITMESSAGE(IRAT,IRAT1,IROW,MATH(3)%NROW,'Progress ')
  IF(IBATCH.EQ.1)WRITE(6,'(A,F10.3,A)') '+Progress ',REAL(IROW*100)/REAL(MATH(3)%NROW),'%'
 ENDDO

 IF(IEFUNC.EQ.4)CALL GXG1GTLEG_WRITE(TRIM(PREFVAL(1))//'\tmp\gt.leg')
 
 LEX=.TRUE.

 IF(IBATCH.EQ.0)CALL UTL_MESSAGEHANDLE(1)

 END SUBROUTINE MATH1COMP

 !###======================================================================
 SUBROUTINE MATH1CALCVALUE(XC,YC,IOP,CLC,LCOMP,IDFRESULT,IEFUNC)
 !###======================================================================
 IMPLICIT NONE
 LOGICAL,INTENT(IN) :: LCOMP
 INTEGER,INTENT(IN),DIMENSION(3) :: IOP
 INTEGER,INTENT(IN) :: IEFUNC
 REAL,INTENT(IN),DIMENSION(3) :: CLC
 REAL,INTENT(IN) :: XC,YC
 REAL,INTENT(OUT) :: IDFRESULT
 INTEGER :: IROW,ICOL,I
 REAL :: SV
 REAL,DIMENSION(2) :: IDFVAL
 LOGICAL :: LEX

 IDFRESULT=MATH(3)%NODATA

 !## get values
 DO I=1,2
  !## file opened
  IF(MATH(I)%IU.GT.0)THEN

   !## get irow,icol for current xc/yc
   CALL IDFIROWICOL(MATH(I),IROW,ICOL,XC,YC)
   IF(ICOL.GT.0.AND.ICOL.LE.MATH(I)%NCOL.AND. &
      IROW.GT.0.AND.IROW.LE.MATH(I)%NROW)THEN
    IDFVAL(I)=IDFGETVAL(MATH(I),IROW,ICOL)
   ELSE
    IDFVAL(I)=MATH(I)%NODATA
   ENDIF
  ENDIF

  !## use nodata value as nodata_value
  IF(INODATA.EQ.1)THEN
   IF(IDFVAL(I).EQ.MATH(I)%NODATA)IDFVAL(I)=NODATA_VALUE
  ENDIF
  
 ENDDO

 IF(.NOT.LCOMP)THEN; IDFRESULT=IDFVAL(1); RETURN; ENDIF

 !## groundwater classification
 IF(IEFUNC.EQ.4)THEN; IDFRESULT=GXG1GETGT(IDFVAL(2),IDFVAL(1),MATH(3)%NODATA); RETURN; ENDIF
 
 !## pre-multiply values
 DO I=1,2
  IF(MATH(I)%IU.GT.0)then
   !## ignore nodata value
   LEX=.TRUE.; IF(INODATA.EQ.0.AND.IDFVAL(I).EQ.MATH(I)%NODATA)LEX=.FALSE.
   IF(LEX)THEN
    SELECT CASE (IOP(I))
     CASE (1) ![-]
      IDFVAL(I)=CLC(I)-IDFVAL(I)
     CASE (2) ![+]
      IDFVAL(I)=CLC(I)+IDFVAL(I)
     CASE (3) ![/]
      !## devide by zero yields nodata
      IF(IDFVAL(I).NE.0.0)THEN
       IDFVAL(I)=CLC(I)/IDFVAL(I)
      ELSE
       IDFVAL(I)=MATH(I)%NODATA
      ENDIF
     CASE (4) ![*]
      IDFVAL(I)=CLC(I)*IDFVAL(I)
    END SELECT
   ENDIF
  ENDIF
 END DO

 !## c=a or c=b
 IF(IOP(3).EQ.0)THEN
  IF(MATH(1)%IU.GT.0)THEN
   !## ignore nodata
   IF(INODATA.EQ.0)THEN
    IF(IDFVAL(1).NE.MATH(1)%NODATA)IDFRESULT=IDFVAL(1)
   ELSE
    IDFRESULT=IDFVAL(1)
   ENDIF
  ELSEIF(MATH(2)%IU.GT.0)THEN
   !## ignore-nodata
   IF(INODATA.EQ.0)THEN
    IF(IDFVAL(2).NE.MATH(2)%NODATA)IDFRESULT=IDFVAL(2)
   ELSE
    IDFRESULT=IDFVAL(2)
   ENDIF
  ENDIF
  RETURN
 ENDIF

 !# apply sgn()
 IF(IEFUNC.EQ.5)THEN

  IF(IDFVAL(1).EQ.MATH(1)%NODATA.OR.IDFVAL(2).EQ.MATH(2)%NODATA)THEN
   SV=MATH(3)%NODATA
  ELSE
   SV=0.0
   IF(IDFVAL(1).LT.0.0.AND.IDFVAL(2).LT.0.0)THEN
    IF(ABS(IDFVAL(1)-IDFVAL(2)).GE.TRIM_VALUE)THEN
     IF(IDFVAL(2).LT.IDFVAL(1))SV=1.0 !## increase negative
     IF(IDFVAL(2).GT.IDFVAL(1))SV=2.0 !## decrease negative
    ENDIF
   ELSEIF(IDFVAL(1).GT.0.0.AND.IDFVAL(2).GT.0.0)THEN
    IF(ABS(IDFVAL(1)-IDFVAL(2)).GE.TRIM_VALUE)THEN
     IF(IDFVAL(2).GT.IDFVAL(1))SV=3.0 !## decrease positive
     IF(IDFVAL(2).LT.IDFVAL(1))SV=4.0 !## increase positive
    ENDIF
   ELSEIF(IDFVAL(1).LT.0.0.AND.IDFVAL(2).GT.0.0)THEN
    SV=6.0
   ELSEIF(IDFVAL(1).GT.0.0.AND.IDFVAL(2).LT.0.0)THEN
    SV=5.0
   ENDIF
  ENDIF
  IDFRESULT=SV; RETURN
 ENDIF
 
 !## get final idf-value for child-idf
 SELECT CASE (IOP(3))
  !## difference[-]
  CASE (1)
   IF(INODATA.EQ.0)THEN
    IF(IDFVAL(1).NE.MATH(1)%NODATA.AND. &
       IDFVAL(2).NE.MATH(2)%NODATA)THEN
     IDFRESULT=IDFVAL(1)-IDFVAL(2)
    ENDIF
   ELSE
    IDFRESULT=IDFVAL(1)-IDFVAL(2)
   ENDIF
  !## addition[+]
  CASE(2)
   IF(INODATA.EQ.0)THEN
    IF(IDFVAL(1).NE.MATH(1)%NODATA.AND. &
       IDFVAL(2).NE.MATH(2)%NODATA)THEN
     IDFRESULT=IDFVAL(1)+IDFVAL(2)
    ENDIF
   ELSE
    IDFRESULT=IDFVAL(1)+IDFVAL(2)
   ENDIF
  !## partition[/]
  CASE(3)
   IF(IDFVAL(2).NE.0.0)THEN
    IF(INODATA.EQ.0)THEN
     IF(IDFVAL(1).NE.MATH(1)%NODATA.AND. &
        IDFVAL(2).NE.MATH(2)%NODATA)THEN
      IF(IDFVAL(2).NE.0.0)IDFRESULT=IDFVAL(1)/IDFVAL(2)
     ENDIF
    ELSE
     IF(IDFVAL(2).NE.0.0)IDFRESULT=IDFVAL(1)/IDFVAL(2)
    ENDIF
   ENDIF
  !## multiplication[*]
  CASE(4)
   IF(INODATA.EQ.0)THEN
    IF(IDFVAL(1).NE.MATH(1)%NODATA.AND. &
       IDFVAL(2).NE.MATH(2)%NODATA)THEN
     IDFRESULT=IDFVAL(1)*IDFVAL(2)
    ENDIF
   ELSE
    IDFRESULT=IDFVAL(1)*IDFVAL(2)
   ENDIF
  !## or[?]
  CASE(5)
   IF(INODATA.EQ.0)THEN
    IF(IDFVAL(1).NE.MATH(1)%NODATA)IDFRESULT=IDFVAL(1)
    IF(IDFVAL(2).NE.MATH(2)%NODATA)IDFRESULT=IDFVAL(2)
   ENDIF
  !## smallest
  CASE(6)
   IF(INODATA.EQ.0)THEN
    IF(IDFVAL(1).NE.MATH(1)%NODATA.AND. &
       IDFVAL(2).NE.MATH(2)%NODATA)THEN
     IDFRESULT=MIN(IDFVAL(1),IDFVAL(2))
    ENDIF
   ELSE
    IDFRESULT=MIN(IDFVAL(1),IDFVAL(2))
   ENDIF
  !## biggest
  CASE(7)
   IF(INODATA.EQ.0)THEN
    IF(IDFVAL(1).NE.MATH(1)%NODATA.AND. &
       IDFVAL(2).NE.MATH(2)%NODATA)THEN
     IDFRESULT=MAX(IDFVAL(1),IDFVAL(2))
    ENDIF
   ELSE
    IDFRESULT=MAX(IDFVAL(1),IDFVAL(2))
   ENDIF
 END SELECT
 
 IF(ABS(IDFRESULT).LT.TRIM_VALUE)IDFRESULT=MATH(3)%NODATA
 
 END SUBROUTINE MATH1CALCVALUE

 !###======================================================================
 SUBROUTINE MATH1_PWTCOUNT(DIR,SDLFNAME,ILAYFNAME,SDATE,EDATE,OUTPUTIDF)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR,SDLFNAME,OUTPUTIDF,ILAYFNAME
 INTEGER,INTENT(IN) :: SDATE,EDATE
 CHARACTER(LEN=256),DIMENSION(:),POINTER :: LISTNAME
 CHARACTER(LEN=52) :: WC
 CHARACTER(LEN=52) :: IDFNAME
 INTEGER :: IDATE,ILAY,I,J,IROW,ICOL,NF,MAXILAY,N
 REAL,ALLOCATABLE,DIMENSION(:) :: DT
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IDATES,IPOS
 TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: SDLIDF,IDF,ILAYIDF
 
 ALLOCATE(SDLIDF(2),ILAYIDF(1))
 DO I=1,SIZE(SDLIDF) ; CALL IDFNULLIFY(SDLIDF(I)) ; ENDDO
 DO I=1,SIZE(ILAYIDF); CALL IDFNULLIFY(ILAYIDF(I)); ENDDO

 !## get layer number one to be leading for all others
 WC=TRIM(DIR(INDEX(DIR,'\',.TRUE.)+1:))//'*_L1.IDF'
 IF(.NOT.UTL_DIRINFO_POINTER(DIR,WC,LISTNAME,'F'))RETURN

 N=SIZE(LISTNAME); ALLOCATE(IDATES(N),IPOS(N),DT(N))
 !## sort files
 NF=0; DO I=1,SIZE(LISTNAME)
  !## capatalize them
  LISTNAME(I)=UTL_CAP(LISTNAME(I),'U')
  IDATE=UTL_IDFGETDATE(LISTNAME(I))
  IF(IDATE.NE.0)THEN
   IDATE=UTL_IDATETOJDATE(IDATE)
   !## within given date constraints
   IF(IDATE.GE.SDATE.AND.IDATE.LE.EDATE)THEN
    NF=NF+1; IDATES(NF)=IDATE; IPOS(NF)=I
   ENDIF
  ENDIF
 ENDDO
 IF(NF.LE.0)STOP 'no files found that meet date requirement'
 
 CALL UTL_QKSORT_INT2(IDATES,IPOS,NF,NF)
 DT=0.0; DO I=1,NF
  IF(I.EQ.1) DT(I)=IDATES(I)-SDATE
  IF(I.EQ.NF)DT(I)=EDATE-IDATES(I)
  IF(I.NE.NF)DT(I)=DT(I)+(IDATES(I+1)-IDATES(I))  /2.0
  IF(I.NE.1) DT(I)=DT(I)+(IDATES(I)  -IDATES(I-1))/2.0
 ENDDO
 
 WRITE(*,*) SUM(DT)
 
 !## try to read one idf to get dimensions
 ALLOCATE(IDF(1)); DO I=1,SIZE(IDF); CALL IDFNULLIFY(IDF(I)); ENDDO
 I=IPOS(1); IF(.NOT.IDFREAD(IDF(1),TRIM(DIR)//'\'//TRIM(LISTNAME(I)),0))THEN; ENDIF; CLOSE(IDF(1)%IU)

 !## read/scale topsdl and ilayer
 CALL IDFCOPY(IDF(1),SDLIDF(1)); CALL IDFCOPY(IDF(1),SDLIDF(2)); CALL IDFCOPY(IDF(1),ILAYIDF(1))
 WRITE(*,'(A)') 'Reading/Scaling '//TRIM(SDLFNAME)//'...'
 IF(.NOT.IDFREADSCALE(SDLFNAME,SDLIDF(1),2,1,0.0,0))STOP 'ERROR reading/scaling'
 IF(.NOT.IDFALLOCATEX(SDLIDF(2)))STOP 'ERROR allocating memory'; SDLIDF(2)%X=0.0; SDLIDF(2)%NODATA=0.0
 WRITE(*,'(A)') 'Reading/Scaling '//TRIM(ILAYFNAME)//'...'
 IF(.NOT.IDFREADSCALE(ILAYFNAME,ILAYIDF(1),7,1,0.0,0))STOP 'ERROR reading/scaling'

 !## get minimal/maximum number of modellayers to be processed
 MAXILAY=0; DO IROW=1,SDLIDF(1)%NROW; DO ICOL=1,SDLIDF(1)%NCOL
  IF(ILAYIDF(1)%X(ICOL,IROW).NE.ILAYIDF(1)%NODATA)MAXILAY=MAX(MAXILAY,INT(ILAYIDF(1)%X(ICOL,IROW))+1)
 ENDDO; ENDDO
 CALL IDFDEALLOCATE(IDF,SIZE(IDF)); DEALLOCATE(IDF)
 ALLOCATE(IDF(MAXILAY)); DO I=1,SIZE(IDF); CALL IDFNULLIFY(IDF(I)); ENDDO

 !## process all listed files
 DO J=1,NF 
  I=IPOS(J)

  DO ILAY=1,MAXILAY
   IDFNAME=UTL_SUBST(LISTNAME(I),'L1.IDF','L'//TRIM(ITOS(ILAY))//'.IDF')
   WRITE(*,'(A,F10.2)') 'Reading '//TRIM(IDFNAME)//', deltaT (days): ',DT(J)
   IF(.NOT.IDFREAD(IDF(ILAY),TRIM(DIR)//'\'//TRIM(IDFNAME),1))EXIT
   IF(ILAY.GT.1)THEN; IF(.NOT.IDFEQUAL(IDF(1),IDF(ILAY),1))EXIT; ENDIF
  ENDDO
  IF(ILAY.LE.MAXILAY)STOP 'IDF"s not equal'
  
  DO IROW=1,SDLIDF(1)%NROW; DO ICOL=1,SDLIDF(1)%NCOL
   !## available top for pwt element
   IF(SDLIDF(1)%X(ICOL,IROW).NE.SDLIDF(1)%NODATA)THEN
    !## get modellayer
    ILAY=ILAYIDF(1)%X(ICOL,IROW)
    IF(ILAY.NE.ILAYIDF(1)%NODATA)THEN
     IF(IDF(ILAY)  %X(ICOL,IROW).GT.SDLIDF(1)%X(ICOL,IROW).AND. &
        IDF(ILAY+1)%X(ICOL,IROW).LT.SDLIDF(1)%X(ICOL,IROW))THEN
      SDLIDF(2)%X(ICOL,IROW)=SDLIDF(2)%X(ICOL,IROW)+DT(J) !I)
     ENDIF
    ENDIF
   ENDIF
  ENDDO; ENDDO
  CALL IDFDEALLOCATE(IDF,SIZE(IDF))
 ENDDO

 IF(N.GT.0)THEN; IF(IDFWRITE(SDLIDF(2),OUTPUTIDF,1))THEN; ENDIF; ENDIF
 
 DEALLOCATE(LISTNAME); CALL IDFDEALLOCATE(SDLIDF,SIZE(SDLIDF)); DEALLOCATE(SDLIDF)
 CALL IDFDEALLOCATE(ILAYIDF,SIZE(ILAYIDF)); DEALLOCATE(ILAYIDF)
 CALL IDFDEALLOCATE(IDF,SIZE(IDF)); DEALLOCATE(IDF)
 DEALLOCATE(IDATES,IPOS,DT)
 
 END SUBROUTINE MATH1_PWTCOUNT
 
 END MODULE MOD_MATH_CALC
