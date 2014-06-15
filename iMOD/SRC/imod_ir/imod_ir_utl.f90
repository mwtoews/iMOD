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
MODULE MOD_IR_UTL

USE MOD_IR_PAR
USE MOD_POLYGON_PAR

CONTAINS

 !###======================================================================
 SUBROUTINE IR1ALLOCATE_INIT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J

 !##-----------
 !## allocate memory and nullify pointers for TARGETS
 !##-----------

 ALLOCATE(TTREE(MAXTARGET))
 !## nullify pointers
 DO I=1,MAXTARGET
  NULLIFY(TTREE(I)%POL)
  ALLOCATE(TTREE(I)%POL(MAXSHAPES))
  !##nullify pointers
  DO J=1,MAXSHAPES
   NULLIFY(TTREE(I)%POL(J)%DEF)
   NULLIFY(TTREE(I)%POL(J)%X)
   NULLIFY(TTREE(I)%POL(J)%Y)
   !ALLOCATE(TTREE(I)%POL(J)%X(1))
   !ALLOCATE(TTREE(I)%POL(J)%Y(1))
   !ALLOCATE(TTREE(I)%POL(J)%DEF(1))
   !## initialize number of definitions
   TTREE(I)%POL(J)%NDEF=0
   TTREE(I)%POL(J)%NCRD=0
  END DO
  !## initialize number of polygons
  TTREE(I)%NPOL=0
 END DO
 TTREE%IDPOS    =0
 TTREE%TARGET_ID=0
 NTARGET        =0

 !##-----------
 !## allocate memory and nullify pointers for MEASURES
 !##-----------

 ALLOCATE(MTREE(MAXMEASURE))
 !## nullify pointers
 DO I=1,MAXMEASURE
  NULLIFY(MTREE(I)%POL)
  NULLIFY(MTREE(I)%OPT)
  ALLOCATE(MTREE(I)%POL(MAXSHAPES))
  !##nullify pointers
  DO J=1,MAXSHAPES
   NULLIFY(MTREE(I)%POL(J)%MES)
   NULLIFY(MTREE(I)%POL(J)%X)
   NULLIFY(MTREE(I)%POL(J)%Y)
   !ALLOCATE(MTREE(I)%POL(J)%X(1))
   !ALLOCATE(MTREE(I)%POL(J)%Y(1))
   !ALLOCATE(MTREE(I)%POL(J)%MES(1))
   !## initialize number of measures
   MTREE(I)%POL(J)%NMES=0
   MTREE(I)%POL(J)%NCRD=0
  END DO
  !## initialize number of polygons
  MTREE(I)%NPOL=0
 END DO
 MTREE%IDPOS     =0
 MTREE%MEASURE_ID=0
 NMEASURE        =0

 !##-----------
 !## allocate memory and nullify pointers for RESULTS
 !##-----------

 ALLOCATE(RTREE(MAXRESULT))
 !## nullify pointers
 DO I=1,MAXRESULT
 END DO
 RTREE%IDPOS    =0
 RTREE%RESULT_ID=0
 NRESULT        =0

 END SUBROUTINE IR1ALLOCATE_INIT

 !###======================================================================
 SUBROUTINE IR1DEALLOCATE_TREE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 IF(ALLOCATED(TTREE))THEN
  DO I=1,SIZE(TTREE)
   CALL IR1DEALLOCATE_TARGET(I)
  END DO
  DEALLOCATE(TTREE)
 ENDIF
 IF(ALLOCATED(MTREE))THEN
  DO I=1,SIZE(MTREE)
   CALL IR1DEALLOCATE_MEASURE(I)
  END DO
  DEALLOCATE(MTREE)
 ENDIF
 IF(ALLOCATED(RTREE))THEN
!  DO I=1,SIZE(RTREE)
!   CALL IR1DEALLOCATE_RESULT(I)
!  END DO
  DEALLOCATE(RTREE)
 ENDIF

 END SUBROUTINE IR1DEALLOCATE_TREE

 !###======================================================================
 SUBROUTINE IR1DEALLOCATE_TARGET(IFIELD)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IFIELD
 INTEGER :: IPOL

 IF(ASSOCIATED(TTREE(IFIELD)%POL))THEN
  DO IPOL=1,SIZE(TTREE(IFIELD)%POL)
   CALL IR1DEALLOCATE_TARGET2(IFIELD,IPOL)
  END DO
  DEALLOCATE(TTREE(IFIELD)%POL)
 ENDIF

 END SUBROUTINE IR1DEALLOCATE_TARGET

 !###======================================================================
 SUBROUTINE IR1DEALLOCATE_TARGET2(IFIELD,IPOL)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IFIELD,IPOL

 IF(TTREE(IFIELD)%POL(IPOL)%NDEF.GT.0)THEN
! IF(ASSOCIATED(TTREE(IFIELD)%POL(IPOL)%DEF))
  DEALLOCATE(TTREE(IFIELD)%POL(IPOL)%DEF)
 ENDIF
 IF(TTREE(IFIELD)%POL(IPOL)%NCRD.GT.0)THEN
! IF(ASSOCIATED(TTREE(IFIELD)%POL(IPOL)%X))
  DEALLOCATE(TTREE(IFIELD)%POL(IPOL)%X)
! IF(ASSOCIATED(TTREE(IFIELD)%POL(IPOL)%Y))
  DEALLOCATE(TTREE(IFIELD)%POL(IPOL)%Y)
 ENDIF
 NULLIFY(TTREE(IFIELD)%POL(IPOL)%X)
 NULLIFY(TTREE(IFIELD)%POL(IPOL)%Y)
 NULLIFY(TTREE(IFIELD)%POL(IPOL)%DEF)
 
 TTREE(IFIELD)%POL(IPOL)%NDEF=0

 END SUBROUTINE IR1DEALLOCATE_TARGET2

 !###======================================================================
 SUBROUTINE IR1DEALLOCATE_MEASURE(IFIELD)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IFIELD
 INTEGER :: IPOL

 IF(ASSOCIATED(MTREE(IFIELD)%POL))THEN
  DO IPOL=1,SIZE(MTREE(IFIELD)%POL)
   CALL IR1DEALLOCATE_MEASURE2(IFIELD,IPOL)
  END DO
  DEALLOCATE(MTREE(IFIELD)%POL)
 ENDIF
 IF(ASSOCIATED(MTREE(IFIELD)%OPT))DEALLOCATE(MTREE(IFIELD)%OPT)
 
 END SUBROUTINE IR1DEALLOCATE_MEASURE

 !###======================================================================
 SUBROUTINE IR1DEALLOCATE_MEASURE2(IFIELD,IPOL)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IFIELD,IPOL
 
 IF(MTREE(IFIELD)%POL(IPOL)%NMES.GT.0)THEN
! IF(ASSOCIATED(MTREE(IFIELD)%POL(IPOL)%MES))
  DEALLOCATE(MTREE(IFIELD)%POL(IPOL)%MES)
 ENDIF
 IF(MTREE(IFIELD)%POL(IPOL)%NCRD.GT.0)THEN
! IF(ASSOCIATED(MTREE(IFIELD)%POL(IPOL)%X))
  DEALLOCATE(MTREE(IFIELD)%POL(IPOL)%X)
! IF(ASSOCIATED(MTREE(IFIELD)%POL(IPOL)%Y))
  DEALLOCATE(MTREE(IFIELD)%POL(IPOL)%Y)
 ENDIF
 NULLIFY(MTREE(IFIELD)%POL(IPOL)%X)
 NULLIFY(MTREE(IFIELD)%POL(IPOL)%Y)
 NULLIFY(MTREE(IFIELD)%POL(IPOL)%MES)
 
 MTREE(IFIELD)%POL(IPOL)%NMES=0

 END SUBROUTINE IR1DEALLOCATE_MEASURE2

 !###======================================================================
 REAL FUNCTION IR1IMPULSEFACTOR(IMP,IIR)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IIR
 REAL,INTENT(IN) :: IMP

 IR1IMPULSEFACTOR=(IMP-IR(IIR)%MINIR)/(IR(IIR)%MAXIR-IR(IIR)%MINIR)

 END FUNCTION IR1IMPULSEFACTOR

 !###======================================================================
 REAL FUNCTION IR1FACTORIMPULSE(IMP,IIR)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IIR
 REAL,INTENT(IN) :: IMP

 IR1FACTORIMPULSE=(IMP*(IR(IIR)%MAXIR-IR(IIR)%MINIR))+IR(IIR)%MINIR

 END FUNCTION IR1FACTORIMPULSE

 !###======================================================================
 SUBROUTINE IR1DIRNAME(DIRNAME)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(OUT) :: DIRNAME
 CHARACTER(LEN=MAXLEN),DIMENSION(3) :: CTREE !,PRIVATE
 INTEGER :: IFIELD,ITREE

 !## get level of treeview
 CALL IR1GETTREEVIEWID(ITREE,IFIELD)
 !## construct dirname
 CALL IR1FIELDS_STRING(CTREE,ITREE,IFIELD)
 !## construct result name
 DIRNAME=TRIM(RESDIR)//'\'//TRIM(ADJUSTL(CTREE(1)))//'\'//TRIM(ADJUSTL(CTREE(2)))//'\'//TRIM(ADJUSTL(CTREE(3)))

 END SUBROUTINE

 !###======================================================================
 SUBROUTINE IR1SHAPE2POL(ITREE,IFIELD)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITREE,IFIELD
 INTEGER :: I !,IOS
 
 SELECT CASE (ITREE)
  !## target
  CASE (1)

   CALL WDIALOGSELECT(ID_DIR_PMTAB1TAB2)
   CALL WDIALOGGETMENU(IDF_MENU1,SHPIACT)

   !## copy shapes into polygon for ir
   DO I=1,SHPNO
!    lex=ASSOCIATED(TTREE(IFIELD)%POL(I)%X)
    IF(TTREE(IFIELD)%POL(I)%NCRD.GT.0)THEN
!    if(lex)then
!    IF(ASSOCIATED(TTREE(IFIELD)%POL(I)%X))THEN
     DEALLOCATE(TTREE(IFIELD)%POL(I)%X) !,STAT=IOS)
!    ENDIF
!    lex=ASSOCIATED(TTREE(IFIELD)%POL(I)%Y)
!    IF(LEX)THEN !IF(ASSOCIATED(TTREE(IFIELD)%POL(I)%Y))THEN
     DEALLOCATE(TTREE(IFIELD)%POL(I)%Y) !,STAT=IOS)
    ENDIF
    !NULLIFY(TTREE(IFIELD)%POL(I)%X)
    !NULLIFY(TTREE(IFIELD)%POL(I)%Y)
    ALLOCATE(TTREE(IFIELD)%POL(I)%X(SHPNCRD(I)))
    ALLOCATE(TTREE(IFIELD)%POL(I)%Y(SHPNCRD(I)))
    TTREE(IFIELD)%POL(I)%X      =SHPXC(1:SHPNCRD(I),I)
    TTREE(IFIELD)%POL(I)%Y      =SHPYC(1:SHPNCRD(I),I)
    TTREE(IFIELD)%POL(I)%NCRD   =SHPNCRD(I)
    TTREE(IFIELD)%POL(I)%ITYPE  =SHPTYPE(I)
    TTREE(IFIELD)%POL(I)%IACT   =SHPIACT(I)
    TTREE(IFIELD)%POL(I)%ICLR   =SHPCOLOR(I)
    TTREE(IFIELD)%POL(I)%POLNAME=SHPNAME(I)
    TTREE(IFIELD)%POL(I)%WIDTH  =SHPWIDTH(I)
   END DO
   TTREE(IFIELD)%NPOL=SHPNO

  !## measure
  CASE (2)

   CALL WDIALOGSELECT(ID_DIR_PMTAB2TAB2)
   CALL WDIALOGGETMENU(IDF_MENU1,SHPIACT)

   !## copy shapes into polygon for ir
   DO I=1,SHPNO
    IF(MTREE(IFIELD)%POL(I)%NCRD.GT.0)THEN
!    IF(ASSOCIATED(MTREE(IFIELD)%POL(I)%X))
     DEALLOCATE(MTREE(IFIELD)%POL(I)%X)
!    IF(ASSOCIATED(MTREE(IFIELD)%POL(I)%Y))
     DEALLOCATE(MTREE(IFIELD)%POL(I)%Y)
    ENDIF
    NULLIFY(MTREE(IFIELD)%POL(I)%X)
    NULLIFY(MTREE(IFIELD)%POL(I)%Y)
    ALLOCATE(MTREE(IFIELD)%POL(I)%X(SHPNCRD(I)))
    ALLOCATE(MTREE(IFIELD)%POL(I)%Y(SHPNCRD(I)))
    MTREE(IFIELD)%POL(I)%X      =SHPXC(1:SHPNCRD(I),I)
    MTREE(IFIELD)%POL(I)%Y      =SHPYC(1:SHPNCRD(I),I)
    MTREE(IFIELD)%POL(I)%NCRD   =SHPNCRD(I)
    MTREE(IFIELD)%POL(I)%ITYPE  =SHPTYPE(I)
    MTREE(IFIELD)%POL(I)%IACT   =SHPIACT(I)
    MTREE(IFIELD)%POL(I)%ICLR   =SHPCOLOR(I)
    MTREE(IFIELD)%POL(I)%POLNAME=SHPNAME(I)
    MTREE(IFIELD)%POL(I)%WIDTH  =SHPWIDTH(I)
!WRITE(*,*) 'shape2pol,ifield,ncrd(2)',ifield,SHPNCRD(I),MTREE(IFIELD)%POL(I)%NCRD
   END DO
   MTREE(IFIELD)%NPOL=SHPNO
!   WRITE(*,*) 'shape2pol,field,shpno',ifield,shpno

 END SELECT

 !## clear existence of polygons
 SHPNO=0

 END SUBROUTINE IR1SHAPE2POL

 !###======================================================================
 SUBROUTINE IR1POL2SHAPE(ITREE,IFIELD)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITREE,IFIELD
 INTEGER :: I

 SELECT CASE (ITREE)
  !## target
  CASE (1)
   SHPNO=TTREE(IFIELD)%NPOL
   !## copy shapes into polygon for ir
   DO I=1,SHPNO
    SHPNCRD(I)           =TTREE(IFIELD)%POL(I)%NCRD
    SHPTYPE(I)           =TTREE(IFIELD)%POL(I)%ITYPE
    SHPIACT(I)           =TTREE(IFIELD)%POL(I)%IACT
    SHPCOLOR(I)          =TTREE(IFIELD)%POL(I)%ICLR
    SHPNAME(I)           =TTREE(IFIELD)%POL(I)%POLNAME
    SHPWIDTH(I)          =TTREE(IFIELD)%POL(I)%WIDTH
    SHPXC(1:SHPNCRD(I),I)=TTREE(IFIELD)%POL(I)%X
    SHPYC(1:SHPNCRD(I),I)=TTREE(IFIELD)%POL(I)%Y
   END DO
  !## measure
  CASE (2)
   SHPNO=MTREE(IFIELD)%NPOL
   !## copy shapes into polygon for ir
   DO I=1,SHPNO
    SHPNCRD(I)           =MTREE(IFIELD)%POL(I)%NCRD
    SHPTYPE(I)           =MTREE(IFIELD)%POL(I)%ITYPE
    SHPIACT(I)           =MTREE(IFIELD)%POL(I)%IACT
    SHPCOLOR(I)          =MTREE(IFIELD)%POL(I)%ICLR
    SHPNAME(I)           =MTREE(IFIELD)%POL(I)%POLNAME
    SHPWIDTH(I)          =MTREE(IFIELD)%POL(I)%WIDTH
!WRITE(*,*) 'pol2shape=',shpncrd(i),SIZE(mtree(ifield)%pol(i)%x),ifield
    SHPXC(1:SHPNCRD(I),I)=MTREE(IFIELD)%POL(I)%X
    SHPYC(1:SHPNCRD(I),I)=MTREE(IFIELD)%POL(I)%Y
   END DO
!   WRITE(*,*) 'pol2shape,shpno=',shpno,ifield,MTREE(IFIELD)%NPOL

 END SELECT

 END SUBROUTINE IR1POL2SHAPE

 !###======================================================================
 SUBROUTINE IR1GETTREEVIEWID(ITREE,IFIELD)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: ITREE,IFIELD
 INTEGER :: ID

 CALL WDIALOGSELECT(ID_DIR_PM)
 CALL WDIALOGGETTREEVIEW(IDF_TREEVIEW1,ID)
 CALL IR1GETTREEID(ITREE,IFIELD,ID)

 END SUBROUTINE IR1GETTREEVIEWID

 !###======================================================================
 SUBROUTINE IR1GETTREEID(ITREE,IFIELD,ID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: ITREE,IFIELD
 INTEGER,INTENT(IN) :: ID

 ITREE=1
 DO IFIELD=1,NTARGET
  IF(TTREE(IFIELD)%TARGET_ID.EQ.ID)  RETURN
 END DO
 ITREE=2
 DO IFIELD=1,NMEASURE
  IF(MTREE(IFIELD)%MEASURE_ID.EQ.ID)RETURN
 END DO
 ITREE=3
 DO IFIELD=1,NRESULT
  IF(RTREE(IFIELD)%RESULT_ID.EQ.ID)  RETURN
 END DO

 END SUBROUTINE

 !###======================================================================
 FUNCTION IR1GETFREETREEID()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: ID,I,J,IR1GETFREETREEID

! DO I=1,NTARGET
!  WRITE(*,*) I,NTARGET,TTREE(I)%TARGET_ID
! END DO
! DO I=1,NMEASURE
!  WRITE(*,*) I,NMEASURE,MTREE(I)%MEASURE_ID
! END DO
! DO I=1,NRESULT
!  WRITE(*,*) I,NRESULT,RTREE(I)%RESULT_ID
! END DO

 ID=0
 DO
  ID=ID+1
  J =0
  DO I=1,NTARGET
   IF(TTREE(I)%TARGET_ID.EQ.ID) J=J+1
  END DO
  DO I=1,NMEASURE
   IF(MTREE(I)%MEASURE_ID.EQ.ID)J=J+1
  END DO
  DO I=1,NRESULT
   IF(RTREE(I)%RESULT_ID.EQ.ID) J=J+1
  END DO
  IF(J.EQ.0)EXIT
 END DO
 IR1GETFREETREEID=ID

 END FUNCTION

 !###======================================================================
 SUBROUTINE IR1GETTREEIDS(IFIELD,ITARGET,IMEASURE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IFIELD
 INTEGER,INTENT(OUT) :: ITARGET,IMEASURE
 INTEGER :: I,ITREE

 !## get measure id
 I=RTREE(IFIELD)%IDPOS
 CALL IR1GETTREEID(ITREE,IMEASURE,I)
 !## get target id
 I=MTREE(IMEASURE)%IDPOS
 CALL IR1GETTREEID(ITREE,ITARGET,I)

 END SUBROUTINE IR1GETTREEIDS

 !###======================================================================
 SUBROUTINE IR1FIELDS_STRING(CTREE,ITREE,IFIELD)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),DIMENSION(3),INTENT(OUT) :: CTREE
 INTEGER,INTENT(IN) :: ITREE,IFIELD
 INTEGER :: JTREE,JFIELD,I,ID

 JTREE =ITREE
 JFIELD=IFIELD

 CTREE=''
 DO I=ITREE,1,-1
  SELECT CASE (JTREE)
   CASE (1)
    CTREE(I)=TTREE(JFIELD)%CNAME
!    ID=TTREE(IFIELD)%IDPOS
   CASE (2)
    CTREE(I)=MTREE(JFIELD)%CNAME
    ID=MTREE(JFIELD)%IDPOS
   CASE (3)
    CTREE(I)=RTREE(JFIELD)%CNAME
    ID=RTREE(JFIELD)%IDPOS
  END SELECT
  CALL IR1GETTREEID(JTREE,JFIELD,ID)
 END DO

 END SUBROUTINE IR1FIELDS_STRING

 !###======================================================================
 SUBROUTINE IR1FIELDS_GETIPERIRES(IDFNAME,IPER,IRES)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: IDFNAME
 INTEGER,INTENT(OUT) :: IPER,IRES

 !## find proper time-information found
 DO IPER=1,NPER
  IF(INDEX(TRIM(IDFNAME),TRIM(PER(IPER)%NAMEPER)).NE.0)EXIT
 END DO
 !## find proper topic-information found
 DO IRES=1,NRES
  IF(INDEX(TRIM(IDFNAME),TRIM(RES(IRES)%NAMERES)).NE.0)EXIT
 END DO

 END SUBROUTINE IR1FIELDS_GETIPERIRES

END MODULE MOD_IR_UTL
