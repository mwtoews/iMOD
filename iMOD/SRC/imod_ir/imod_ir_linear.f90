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
MODULE MOD_IR_LINEAR

USE MOD_IR_PAR
USE MOD_IR_UTL, ONLY : IR1FIELDS_STRING,IR1GETTREEVIEWID,IR1IMPULSEFACTOR,IR1FACTORIMPULSE
USE MOD_SMPLX, ONLY : SMPLX_MAIN
USE MOD_SMPLX_PAR
USE MOD_UTL, ONLY : ITOS,RTOS,UTL_GETUNIT,UTL_CREATEDIR,DBL_IGRINSIDEPOLYGON
USE MOD_IDF, ONLY : IDFGETLOC
USE MOD_OSD, ONLY : OSD_OPEN

CONTAINS

 !###======================================================================
 SUBROUTINE IR1LINEAR_DEALLOCATE()
 !###======================================================================
 IMPLICIT NONE

 IF(ALLOCATED(IN_CON))DEALLOCATE(IN_CON)
 IF(ALLOCATED(XVAR))  DEALLOCATE(XVAR)
 IF(ALLOCATED(XSLK))  DEALLOCATE(XSLK)
 IF(ALLOCATED(ISORT)) DEALLOCATE(ISORT)
 IF(ALLOCATED(IMP))   DEALLOCATE(IMP)
 IF(ALLOCATED(IFIXED))DEALLOCATE(IFIXED)

 END SUBROUTINE

 !###======================================================================
 SUBROUTINE IR1LINEAR_ALLOCATE(IFIELD,JFIELD)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IFIELD,JFIELD
 INTEGER :: IIR,IPOL1,I,IRES,IPER

 !## number of variables to be estimated
 NVAR=0
 DO IIR=1,NIR                  !## number of measures
  IF(IR(IIR)%ISEL.NE.0)THEN
   NVAR=NVAR+MTREE(IFIELD)%NPOL  !## number of measure-polygons
  ENDIF
 ENDDO

!WRITE(*,*) 'NVAR=',NVAR
!WRITE(*,*) ALLOCATED(IRS),ALLOCATED(IPR)

 !## number of conditions
 NCON=0
 DO IPOL1=1,TTREE(JFIELD)%NPOL  !## number of targets
  DO IRES=1,NRES              !## number of results-types
   IF(IRS(IRES).EQ.1)THEN
    DO IPER=1,NPER            !## number of times
     IF(IPR(IPER).EQ.1)NCON=NCON+2  !## two definitions
    ENDDO
   ENDIF
  ENDDO
 ENDDO
 !## extent ncon to capture boundaries upper- and lower limits nvar
! NCON=NCON+(2*NVAR)
 NCON=NCON+(2*NVAR)

! WRITE(*,*) 'NCON=',NCON,TTREE(JFIELD)%NPOL
! WRITE(*,*) 'IPR',IPR
! WRITE(*,*) 'IRS',IRS

 !## NVAR = number of variables to be estimated
 !## NCON = number of conditions
 ALLOCATE(IN_CON(NCON),IMP(NVAR),IFIXED(NVAR))

 !## number of variables to be estimated
 NVAR=0
 DO I=1,MTREE(IFIELD)%NPOL
  DO IIR=1,NIR                  !## number of measures
   IF(IR(IIR)%ISEL.NE.0)THEN
    NVAR=NVAR+1
!    IFIXED(I)=-1*IR(IIR)%IFIXED  !## potential binair
    IFIXED(NVAR)=IR(IIR)%IFIXED  !## potential binair
    IMP   (NVAR)=IR(IIR)%IMP
   ENDIF
  ENDDO
 ENDDO
! do i=1,nvar
!  WRITE(*,*) i,ifixed(i),imp(i)
! end do

 END SUBROUTINE IR1LINEAR_ALLOCATE

 !###======================================================================
 SUBROUTINE IR1LINEARPROGRAMMING(IFIELD,JFIELD,IFT,INEXT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: INEXT
 INTEGER,INTENT(IN) :: IFIELD,JFIELD,IFT
 INTEGER :: I,J,K,L,M,IIR,IPOL1,IPOL2,IPER,IRES,ICON,IMES,IVAR
 REAL(KIND=DP_KIND) :: MAXVAR

 !## in_con
 !## -----------------> nvar*nmeas
 !## |
 !## V ntarget*2 (upper/lower)
 !## |
 !## V nvar*2

 !## initialize in_con
 IN_CON=''

 J   =0
 ICON=0

 !## targets polygons
 DO IPOL1=1,TTREE(JFIELD)%NPOL
  J=J+1

  L=0
  !## type results
  DO IRES=1,NRES
   IF(IRS(IRES).EQ.1)THEN
    L=L+1
    M=0
    !## time results
    DO IPER=1,NPER
     IF(IPR(IPER).EQ.1)THEN
      M=M+1

      ICON          =ICON+1
      IN_CON(ICON)  =''     !## upper boundary
      IN_CON(ICON+1)=''     !## lower boundary
      K=0
      !## measure polygon
      DO IPOL2=1,MTREE(IFIELD)%NPOL
       K=K+1

       I=0
       !## measures
       DO IIR=1,NIR                  !## number of measures
        IF(IR(IIR)%ISEL.NE.0)THEN
         I=I+1

         !## lower boundary
         IN_CON(ICON)=TRIM(IN_CON(ICON))//' '//TRIM(RTOS(COEF(I,J,K,L,M,1),'E',5))
         !## upper boundary
         IN_CON(ICON+1)=TRIM(IN_CON(ICON+1))//' '//TRIM(RTOS(COEF(I,J,K,L,M,2),'E',5))

        ENDIF
       END DO  !DO IIR=1,NIR

      ENDDO  !DO IPOL2=1,POLNO(2,IFIELD)

      !## find constrains - target definition compared to percentile
      DO I=1,TTREE(JFIELD)%POL(IPOL1)%NDEF
       IF(TTREE(JFIELD)%POL(IPOL1)%DEF(I)%INEWT.EQ.IRES.AND. &  !## topic
          TTREE(JFIELD)%POL(IPOL1)%DEF(I)%INEWP.EQ.IPER)THEN    !## period

        !## add lower limit
        IN_CON(ICON)=TRIM(IN_CON(ICON))//' >= '//TRIM(RTOS(TTREE(JFIELD)%POL(IPOL1)%DEF(I)%LOWER,'F',3))
        ICON        =ICON+1
        !## add upper limit
        IN_CON(ICON)=TRIM(IN_CON(ICON))//' <= '//TRIM(RTOS(TTREE(JFIELD)%POL(IPOL1)%DEF(I)%UPPER,'F',3))

       ENDIF
      ENDDO

     ENDIF
    ENDDO  !DO IPER=1,NPER

   ENDIF
  ENDDO  !DO IRES=1,NRES

 ENDDO  !DO IPOL1=1,POLNO(1,IFIELD)

 IVAR=0
 DO I=1,MTREE(IFIELD)%NPOL !NVAR
  DO IIR=1,NIR                  !## number of measures
   IF(IR(IIR)%ISEL.NE.0)THEN
    ICON=ICON+1
    IVAR=IVAR+1
    !## fill x1..,0,0... of ...0,0,x1... etc.
    DO J=1,NVAR
     IF(J.EQ.IVAR)THEN
      IN_CON(ICON)=' '//TRIM(IN_CON(ICON))//' 1.0D0'
     ELSE
      IN_CON(ICON)=' '//TRIM(IN_CON(ICON))//' 0.0D0'
     ENDIF
    END DO

    ICON          =ICON+1
    IN_CON(ICON)  =IN_CON(ICON-1)

    !## reals (value=0) or temporary real to be evaluated yet! (value=-1)
    IF(IFIXED(IVAR).EQ.0.OR.IFIXED(IVAR).EQ.-1)THEN
     IN_CON(ICON-1)=TRIM(IN_CON(ICON-1))//' >= '//TRIM(RTOS(IR1IMPULSEFACTOR(IR(IIR)%LLIMP,IIR),'F',3))
     IN_CON(ICON)  =TRIM(IN_CON(ICON))  //' <= '//TRIM(RTOS(IR1IMPULSEFACTOR(IR(IIR)%ULIMP,IIR),'F',3))
    ELSEIF(IFIXED(IVAR).EQ.1)THEN
     IN_CON(ICON-1)=TRIM(IN_CON(ICON-1))//' >= '//TRIM(RTOS(IR1IMPULSEFACTOR(IMP(IVAR)-0.0D0,IIR),'F',3))
     IN_CON(ICON)  =TRIM(IN_CON(ICON))  //' <= '//TRIM(RTOS(IR1IMPULSEFACTOR(IMP(IVAR)+0.0D0,IIR),'F',3))
    ENDIF
   ENDIF
  END DO
 ENDDO

 NCON=ICON
! DO ICON=1,NCON
!  WRITE(*,*) ICON,TRIM(IN_CON(ICON))!,IFIXED(ICON),IMP(ICON)
! END DO

 !## minimize objective function
 IN_OBJ='-1'
 DO I=2,NVAR
  IN_OBJ=TRIM(IN_OBJ)//' -1'
 END DO

 IF(ALLOCATED(ISORT))DEALLOCATE(ISORT)
 !## perform linearisation algorithm
 CALL SMPLX_MAIN()

 !## copy results to application
 !## fill results in memory
 NVAR=0
 DO IPOL2=1,MTREE(IFIELD)%NPOL
  !## number of measures optimized
  IMES=0
  DO IIR=1,NIR
   IF(IR(IIR)%ISEL.NE.0)THEN
    IMES=IMES+1
    NVAR=NVAR+1
    MTREE(IFIELD)%POL(IPOL2)%NMES=IMES
    MTREE(IFIELD)%POL(IPOL2)%MES(IMES)%IMES=IIR
    !## real
    IF(IFIXED(NVAR).EQ.0)THEN
     IF(IFT.EQ.1)MTREE(IFIELD)%POL(IPOL2)%MES(IMES)%IMP   =IR1FACTORIMPULSE(XVAR(NVAR),IIR)!XVAR(NVAR)*IR(IIR)%MAXIR
     IF(IFT.EQ.2)MTREE(IFIELD)%POL(IPOL2)%MES(IMES)%FT_IMP=IR1FACTORIMPULSE(XVAR(NVAR),IIR)!XVAR(NVAR)*IR(IIR)%MAXIR
    !## binair
    ELSEIF(IFIXED(NVAR).EQ.1)THEN
     IF(IFT.EQ.1)MTREE(IFIELD)%POL(IPOL2)%MES(IMES)%IMP   =IR1FACTORIMPULSE(XVAR(NVAR),IIR)!XVAR(NVAR)*IR(IIR)%MAXIR
     IF(IFT.EQ.2)MTREE(IFIELD)%POL(IPOL2)%MES(IMES)%FT_IMP=IR1FACTORIMPULSE(XVAR(NVAR),IIR)!XVAR(NVAR)*IR(IIR)%MAXIR
    ENDIF
   ENDIF
  ENDDO
 ENDDO

 !## print summary
 CALL IR1LINEAR_CREATEOUTPUT(IFIELD,JFIELD,IFT)

 !## search candidate for real->binair - next iteration?
 INEXT =0
 NVAR  =0
 IVAR  =0
 MAXVAR=1.0D0
 DO IPOL2=1,MTREE(IFIELD)%NPOL
  DO IIR=1,NIR
   IF(IR(IIR)%ISEL.NE.0)THEN
    NVAR=NVAR+1
    IF(IFIXED(NVAR).EQ.-1)THEN
     IF(MIN(XVAR(NVAR),(1.0D0-XVAR(NVAR))).LT.MAXVAR)THEN
      MAXVAR=MIN(XVAR(NVAR),(1.0D0-XVAR(NVAR)))
      IVAR  =NVAR
     ENDIF
    ENDIF
   ENDIF
  ENDDO
 ENDDO
 IF(IVAR.NE.0)THEN
  IFIXED(IVAR)=1
  IMP(IVAR)   =REAL(INT(MAXVAR))
  !## start next iteration
  INEXT=1
 ENDIF

 END SUBROUTINE IR1LINEARPROGRAMMING

 !###======================================================================
 SUBROUTINE IR1LINEAR_CREATEOUTPUT(IFIELD,JFIELD,IFT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IFIELD,JFIELD,IFT
 INTEGER :: IU,IPOL,IMES,IDEF,IIR,INEWP,INEWT,I,ICON,NHOLD,THOLD!,ITREE
 CHARACTER(LEN=256) :: LINE
 REAL(KIND=DP_KIND) :: CIMP,LIMP,UIMP,OLIMP,OUIMP

 !## no output during picard-iteration
 NVAR=0
 DO IPOL=1,MTREE(IFIELD)%NPOL
  DO IMES=1,MTREE(IFIELD)%POL(IPOL)%NMES
   NVAR=NVAR+1
   IF(IFIXED(NVAR).EQ.-1)RETURN
  ENDDO
 ENDDO

 CALL IR1FIELDS_STRING(CTREE,2,IFIELD)
 DIRNAME=TRIM(RESDIR)//'\'//TRIM(ADJUSTL(CTREE(1)))//'\'//TRIM(ADJUSTL(CTREE(2)))!//'\optimize.log'!//TRIM(CNAME)!TRIM(ADJUSTL(CTREE(3)))
 IF(.NOT.IOSDIREXISTS(TRIM(DIRNAME)))CALL UTL_CREATEDIR(DIRNAME)
 DIRNAME=TRIM(DIRNAME)//'\optimize.log'!//TRIM(CNAME)!TRIM(ADJUSTL(CTREE(3)))

 IU=UTL_GETUNIT()
 IF(IFT.EQ.1)THEN
  CALL OSD_OPEN(IU,FILE=DIRNAME,FORM='FORMATTED',STATUS='UNKNOWN')
 ELSE
  CALL OSD_OPEN(IU,FILE=DIRNAME,FORM='FORMATTED',STATUS='OLD',POSITION='APPEND')
 ENDIF

 IF(IFT.EQ.2)WRITE(IU,'(//A/)') '     '//'Alternative solution with NO constraints on measures!'

 NHOLD=0
 THOLD=0
 WRITE(IU,*)
 WRITE(IU,*) '     '//'TARGETS'
 WRITE(IU,'(5X,7A1)') ('-',I=1,7)
 ICON=0
 DO IPOL=1,TTREE(JFIELD)%NPOL
  LINE='Polygon: '//TRIM(ADJUSTL(TTREE(JFIELD)%POL(IPOL)%POLNAME))
  WRITE(IU,*)
  WRITE(IU,*) '     '//TRIM(LINE)
  DO IDEF=1,TTREE(JFIELD)%POL(IPOL)%NDEF

   ICON=ICON+1
   DO I=1,NCON
    IF(ISORT(I).EQ.ICON)OLIMP=XSLK(I)
   END DO
   ICON=ICON+1
   DO I=1,NCON
    IF(ISORT(I).EQ.ICON)OUIMP=XSLK(I)
   END DO

   OLIMP=REAL(NINT(OLIMP*100.0D0))/100.0D0
   OUIMP=REAL(NINT(OUIMP*100.0D0))/100.0D0

   INEWP=TTREE(JFIELD)%POL(IPOL)%DEF(IDEF)%INEWP
   INEWT=TTREE(JFIELD)%POL(IPOL)%DEF(IDEF)%INEWT
   LIMP =TTREE(JFIELD)%POL(IPOL)%DEF(IDEF)%LOWER
   UIMP =TTREE(JFIELD)%POL(IPOL)%DEF(IDEF)%UPPER
   LINE=TRIM(PER(INEWP)%NAMEPER)//' '//TRIM(RES(INEWT)%NAMERES)!//& !//' = '// &

   IF(OLIMP.GE.LIMP.AND.OUIMP.LE.UIMP)THEN
    WRITE(IU,*) '     '//TRIM(LINE)
    NHOLD=NHOLD+1
   ELSE
    WRITE(IU,*) '|x|  '//TRIM(LINE)
   ENDIF
   THOLD=THOLD+1

!   !## heads
    LINE='Average effect is '//TRIM(RTOS(OLIMP,'F',3))//' within polygon area '// &
         '(should be between '//TRIM(RTOS(LIMP,'F',3))//' and '//TRIM(RTOS(UIMP,'F',3))//')'
    WRITE(IU,*) '     '//TRIM(LINE)

  END DO
 ENDDO
 LINE='A solution has been found that meets '//TRIM(ITOS(NHOLD))//' out of '//TRIM(ITOS(THOLD))//' targets'
 WRITE(IU,'(/A)') '     '//TRIM(LINE)
 !## problem not solved
 IF(ICNVG.EQ.1)WRITE(IU,'(A)') '     '//'Status solver: '//TRIM(LPSTATUS)
 WRITE(IU,*)

 WRITE(IU,*) '     '//'MEASURES'
 WRITE(IU,'(5X,8A1)') ('-',I=1,8)

 NVAR=0
 DO IPOL=1,MTREE(IFIELD)%NPOL
  LINE='Polygon: '//TRIM(ADJUSTL(MTREE(IFIELD)%POL(IPOL)%POLNAME))
  WRITE(IU,*)
  WRITE(IU,*) '     '//TRIM(LINE)
  DO IMES=1,MTREE(IFIELD)%POL(IPOL)%NMES
   NVAR=NVAR+1
   IIR =MTREE(IFIELD)%POL(IPOL)%MES(IMES)%IMES
   IF(IFT.EQ.1)CIMP=MTREE(IFIELD)%POL(IPOL)%MES(IMES)%IMP
   IF(IFT.EQ.2)CIMP=MTREE(IFIELD)%POL(IPOL)%MES(IMES)%FT_IMP
   IF(IFIXED(NVAR).EQ.0)THEN
    LIMP=IR(IIR)%LLIMP!*IR(IIR)%MAXIR
    UIMP=IR(IIR)%ULIMP!*IR(IIR)%MAXIR
    LINE=TRIM(IR(IIR)%NAMEIR)//' = '//TRIM(RTOS(CIMP,'F',2))//' ('//TRIM(RTOS(LIMP,'F',2))//' - '//TRIM(RTOS(UIMP,'F',2))//')'
   ELSEIF(IFIXED(NVAR).EQ.1)THEN
    LIMP=IMP(NVAR)
    LINE=TRIM(IR(IIR)%NAMEIR)//' = '//TRIM(RTOS(CIMP,'F',2))//' (fixed: '//TRIM(RTOS(LIMP,'F',2))//')'
   ENDIF
   IF(CIMP.GE.LIMP.AND.CIMP.LE.UIMP)THEN
    WRITE(IU,*) '     '//TRIM(LINE)
   ELSE
    WRITE(IU,*) '|x|  '//TRIM(LINE)
   ENDIF
  END DO
 ENDDO

 CLOSE(IU)

 END SUBROUTINE IR1LINEAR_CREATEOUTPUT

 !###======================================================================
 SUBROUTINE IR1LINEAR_VIEWOUTPUT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: ITREE,IFIELD

 !## get level of treeview
 CALL IR1GETTREEVIEWID(ITREE,IFIELD)
 !## show the logfile
 CALL IR1LINEAR_SHOWOUTPUT(IFIELD)

 END SUBROUTINE

 !###======================================================================
 SUBROUTINE IR1LINEAR_SHOWOUTPUT(IFIELD)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IFIELD
 INTEGER :: IWIN,I,IU
 LOGICAL :: LEX

 CALL IR1FIELDS_STRING(CTREE,2,IFIELD)
 DIRNAME=TRIM(RESDIR)//'\'//TRIM(ADJUSTL(CTREE(1)))//'\'//TRIM(ADJUSTL(CTREE(2)))//'\optimize.log'
 INQUIRE(FILE=DIRNAME,EXIST=LEX)
 IF(.NOT.LEX)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot open logfile'//CHAR(13)// &
   TRIM(DIRNAME),'Error')
  RETURN
 ENDIF

 CALL WINDOWOPENCHILD(IWIN,FLAGS=SYSMENUON,WIDTH=1000,HEIGHT=500)
 CALL WINDOWSELECT(IWIN)
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=DIRNAME,STATUS='OLD',IOSTAT=I)
 IF(I.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot view the created file : '//CHAR(13)// &
   TRIM(DIRNAME)//'.'//CHAR(13)//'It is probably opened already in another application','Error')
 ELSE
  CLOSE(IU)
  CALL WEDITFILE(DIRNAME,ITYPE=MODAL,IDMENU=0, &
                 IFLAGS=NOTOOLBAR+VIEWONLY+WORDWRAP+NOFILENEWOPEN+NOFILESAVEAS,&
                 IFONT=4,ISIZE=10)
 ENDIF

 END SUBROUTINE IR1LINEAR_SHOWOUTPUT

 !###======================================================================
 FUNCTION IR2LINEAR_PERCENTILES(JFIELD,IIR,JPOL,JPER,JRES,IRES)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: JFIELD,IIR,JPOL,JPER,JRES,IRES
 LOGICAL :: IR2LINEAR_PERCENTILES
 REAL(KIND=DP_KIND) :: XC,YC,XL,XU
 INTEGER :: IROW,ICOL,IPOL,NSORT,NCRD
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: XSORT

 IR2LINEAR_PERCENTILES=.FALSE.

 ALLOCATE(XSORT(EFFECT(1)%NCOL*EFFECT(1)%NROW))

 DO IPOL=1,TTREE(JFIELD)%NPOL

  NCRD=TTREE(JFIELD)%POL(IPOL)%NCRD
  NSORT=0

  !## evaluate whether effect are within polygon
  DO IROW=1,EFFECT(1)%NROW
   !## y-mid
   DO ICOL=1,EFFECT(1)%NCOL
    !## x-mid
    !## get xy-coordinates
    CALL IDFGETLOC(EFFECT(1),IROW,ICOL,XC,YC)
    !## check whether current cell is within current polygon
    IF(DBL_IGRINSIDEPOLYGON(XC,YC,TTREE(JFIELD)%POL(IPOL)%X,TTREE(JFIELD)%POL(IPOL)%Y,NCRD).EQ.1)THEN
     NSORT       =NSORT+1
     XSORT(NSORT)=EFFECT(1)%X(ICOL,IROW)
    ENDIF

   END DO
  END DO

  XU=0.0D0
  XL=0.0D0

  !## resulting idf not within curent target polygon, can be happening
  IF(NSORT.GT.0)THEN

   !## heads
   IF(RES(IRES)%ITYPERES.EQ.0)THEN

    XU  =SUM(XSORT(1:NSORT))/REAL(NSORT)
    XL=XU

   !## m/dag (mean)
   ELSEIF(RES(IRES)%ITYPERES.EQ.1)THEN

    XU  =SUM(XSORT(1:NSORT))/REAL(NSORT)
    XL=XU

   ENDIF

  ENDIF

  COEF(IIR,IPOL,JPOL,JRES,JPER,1)=XL
  COEF(IIR,IPOL,JPOL,JRES,JPER,2)=XU

 END DO

 DEALLOCATE(XSORT)

 IR2LINEAR_PERCENTILES=.TRUE.

 END FUNCTION IR2LINEAR_PERCENTILES

END MODULE MOD_IR_LINEAR
