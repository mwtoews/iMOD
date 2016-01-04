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
MODULE MOD_SETTINGS

USE WINTERACTER
USE RESOURCE

USE MODPLOT
USE MOD_UTL, ONLY : UTL_GENLABELSREAD,UTL_GENLABELSDEALLOCATE,NV,VAR
USE MOD_IPF, ONLY : UTL_GETUNITIPF
USE MOD_GENPLOT_PAR, ONLY : GEN,MXGEN 
USE MOD_GENPLOT, ONLY : GENLABELSDEFINE
USE MOD_LEGEND, ONLY : LEG_MAIN,LEG_CREATEINIT
USE MOD_IPF_PAR, ONLY : IPF,NIPF
USE MOD_IFF, ONLY : UTL_GETUNITIFF
USE MOD_IPF_LABEL, ONLY : IMOD3D_LABELS

 CONTAINS

 !###====================================================================
 SUBROUTINE SETTINGS_MAIN(FNAME)
 !###====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: FNAME
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITYPE,IPLOT,IU,NATTRIB,I,IRGB,IOS,CFN_N_ELEM
 LOGICAL :: LIPF,LIFF,LGEN,LISG,LLEG
 CHARACTER(LEN=20),DIMENSION(:),ALLOCATABLE :: ATTRIB
 CHARACTER(LEN=10) :: CAXES
 CHARACTER(LEN=256) :: LINE

 !## find fname in mp()iplot
 IF(PRESENT(FNAME))THEN
  DO IPLOT=1,MXMPLOT
   IF(MP(IPLOT)%IDFNAME(INDEX(MP(IPLOT)%IDFNAME,'\',.TRUE.)+1:).EQ.FNAME)EXIT
  ENDDO
 ELSE
  DO IPLOT=1,MXMPLOT; IF(MP(IPLOT)%ISEL)EXIT; ENDDO
 ENDIF
 !## not for idf's and mdf's
 IF(MP(IPLOT)%IPLOT.EQ.1.OR.MP(IPLOT)%IPLOT.EQ.5)RETURN
 
 LIPF=.FALSE.
 LIFF=.FALSE.
 LGEN=.FALSE.
 LISG=.FALSE.
 IF(MP(IPLOT)%IPLOT.EQ.2)LIPF=.TRUE.
 IF(MP(IPLOT)%IPLOT.EQ.3)LIFF=.TRUE.
 IF(MP(IPLOT)%IPLOT.EQ.4)LISG=.TRUE.
 IF(MP(IPLOT)%IPLOT.EQ.6)LGEN=.TRUE.
 !## isg files
! IF(MP(IPLOT)%IPLOT.EQ.4)THEN
!  CALL SETTINGS_GENSYMBOLS()
!  RETURN
! ENDIF

 IF(LIFF)THEN
  IU=UTL_GETUNITIFF(MP(IPLOT)%IDFNAME,'OLD')
 ELSEIF(LIPF)THEN
  !## clear all ipf-names
  DO I=1,NIPF
   IPF(I)%FNAME=''
  END DO
  IU=UTL_GETUNITIPF(MP(IPLOT)%IDFNAME,'OLD')
!  READ(IU,*)
! ELSEIF(LISG)THEN
 ELSEIF(LGEN)THEN
  CALL UTL_GENLABELSREAD(MP(IPLOT)%IDFNAME (:INDEX(MP(IPLOT)%IDFNAME,'.',.TRUE.)-1)//'.dat' )
 ENDIF
 
 IF(LIFF.OR.LIPF)THEN
  READ(IU,'(A256)',IOSTAT=IOS) LINE
  READ(LINE,*,IOSTAT=IOS) NATTRIB
  IF(IOS.NE.0)THEN
   NATTRIB=CFN_N_ELEM(' ,;',3,LINE)
   ALLOCATE(ATTRIB(NATTRIB))
   READ(LINE,*) (ATTRIB(I),I=1,NATTRIB)  
  ELSE
   IF(LIPF)READ(IU,*) NATTRIB
   ALLOCATE(ATTRIB(NATTRIB))
   DO I=1,NATTRIB
    READ(IU,*) ATTRIB(I)
   ENDDO
  ENDIF
  CLOSE(IU)
 ELSEIF(LGEN)THEN
  NATTRIB=NV
  IF(NATTRIB.GT.0)THEN
   ALLOCATE(ATTRIB(NATTRIB))
   ATTRIB=VAR(:,0)
   CALL UTL_GENLABELSDEALLOCATE()
  ELSE
!   NATTRIB=1
!   ALLOCATE(ATTRIB(NATTRIB))
!   ATTRIB(1)='nothing'
  ENDIF
 ELSEIF(LISG)THEN
  NATTRIB=1
  ALLOCATE(ATTRIB(NATTRIB))
  ATTRIB(1)='nothing yet'
 ENDIF
 
 CALL WDIALOGLOAD(ID_DCONFIGURE)
 IF(LIPF)CALL WDIALOGTITLE('IPF Configure '//TRIM(MP(IPLOT)%ALIAS))
 IF(LIFF)CALL WDIALOGTITLE('IFF Configure '//TRIM(MP(IPLOT)%ALIAS))
 IF(LISG)CALL WDIALOGTITLE('ISG Configure '//TRIM(MP(IPLOT)%ALIAS))
 IF(LGEN)CALL WDIALOGTITLE('GEN Configure '//TRIM(MP(IPLOT)%ALIAS))
 IF(MP(IPLOT)%ILEG.EQ.0)CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO1)
 IF(MP(IPLOT)%ILEG.EQ.1)CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO2)
 LLEG=MP(IPLOT)%ILEG.EQ.1
 CALL WDIALOGCOLOUR(IDF_STRING1,MP(IPLOT)%SCOLOR,MP(IPLOT)%SCOLOR)

 IF(MP(IPLOT)%HCOL.GT.0)    CALL WDIALOGPUTCHECKBOX(IDF_CHECK5,1)
 IF(MP(IPLOT)%HCOL.LE.0)    CALL WDIALOGPUTCHECKBOX(IDF_CHECK5,0)
 CALL WDIALOGPUTCHECKBOX(IDF_CHECK3,MP(IPLOT)%FADEOUT)           !## fadeout
 CALL WDIALOGPUTINTEGER(IDF_INTEGER1,MP(IPLOT)%IDFI)             !## sight depth

 IF(NATTRIB.GT.0)THEN
  CALL WDIALOGPUTMENU(IDF_MENU1,ATTRIB,NATTRIB,MP(IPLOT)%IATTRIB) !## colour-label
 ELSE
  CALL WDIALOGCLEARFIELD(IDF_MENU1) !## colour-label
  CALL WDIALOGFIELDSTATE(IDF_RADIO2,0)
  CALL WDIALOGFIELDSTATE(ID_LABELING,0)
 ENDIF

 IF(MP(IPLOT)%XCOL.LE.0)MP(IPLOT)%XCOL=1
 IF(MP(IPLOT)%YCOL.LE.0)MP(IPLOT)%YCOL=2
 IF(MP(IPLOT)%ZCOL.LE.0)MP(IPLOT)%ZCOL=3
 IF(LIPF)THEN
  CALL WDIALOGPUTMENU(IDF_MENU3,ATTRIB,NATTRIB,MP(IPLOT)%XCOL)   !x
  CALL WDIALOGPUTMENU(IDF_MENU4,ATTRIB,NATTRIB,MP(IPLOT)%YCOL)   !y
  CALL WDIALOGPUTMENU(IDF_MENU5,ATTRIB,NATTRIB,MP(IPLOT)%ZCOL)   !z
  CALL WDIALOGPUTMENU(IDF_MENU7,ATTRIB,NATTRIB,MP(IPLOT)%Z2COL)  !second z
  I=0
  IF(MP(IPLOT)%ZCOL.NE.MP(IPLOT)%Z2COL)I=1
  CALL WDIALOGPUTCHECKBOX(IDF_CHECK6,I)
  IF(MP(IPLOT)%HCOL.GT.0)CALL WDIALOGPUTMENU(IDF_MENU6,ATTRIB,NATTRIB,MP(IPLOT)%HCOL)   !plot
  IF(MP(IPLOT)%HCOL.LE.0)CALL WDIALOGPUTMENU(IDF_MENU6,ATTRIB,NATTRIB,1)   !plot
  WRITE(CAXES,'(10I1)') MP(IPLOT)%IAXES(1:10)
  READ(CAXES,'(I10)') I
  CALL WDIALOGPUTINTEGER(IDF_INTEGER2,I)
  CALL WDIALOGFIELDSTATE(IDF_MENU3,1)
  CALL WDIALOGFIELDSTATE(IDF_MENU4,1)
  CALL WDIALOGFIELDSTATE(IDF_MENU5,1)
  CALL WDIALOGFIELDSTATE(IDF_MENU6,1)
  CALL WDIALOGFIELDSTATE(IDF_INTEGER2,1)
  CALL WDIALOGFIELDSTATE(IDF_LABEL6,1)
 ELSEIF(LIFF)THEN
  CALL WDIALOGPUTMENU(IDF_MENU3,(/'--- not available ---'/),1,1) !ATTRIB,NATTRIB,1)   !x
  CALL WDIALOGPUTMENU(IDF_MENU4,(/'--- not available ---'/),1,1) !,ATTRIB,NATTRIB,2)   !y
  CALL WDIALOGPUTMENU(IDF_MENU5,(/'--- not available ---'/),1,1) !,ATTRIB,NATTRIB,3)   !z
  CALL WDIALOGPUTMENU(IDF_MENU6,(/'--- not available ---'/),1,1) !,ATTRIB,NATTRIB,3)   !z
  CALL WDIALOGPUTMENU(IDF_MENU7,(/'--- not available ---'/),1,1) !,ATTRIB,NATTRIB,3)   !z
  CALL WDIALOGFIELDSTATE(IDF_MENU3,0)
  CALL WDIALOGFIELDSTATE(IDF_MENU4,0)
  CALL WDIALOGFIELDSTATE(IDF_MENU5,0)
  CALL WDIALOGFIELDSTATE(IDF_MENU6,0)
  CALL WDIALOGFIELDSTATE(IDF_CHECK5,0)
  CALL WDIALOGFIELDSTATE(IDF_CHECK6,0)
  CALL WDIALOGFIELDSTATE(IDF_INTEGER2,0)
  CALL WDIALOGFIELDSTATE(IDF_LABEL6,0)
  CALL WDIALOGFIELDSTATE(IDF_LABEL10,0)
  CALL WDIALOGFIELDSTATE(ID_LABELING,0)
 ELSEIF(LGEN.OR.LISG)THEN
  CALL WDIALOGPUTMENU(IDF_MENU3,(/'--- not available ---'/),1,1) !ATTRIB,NATTRIB,1)   !x
  CALL WDIALOGPUTMENU(IDF_MENU4,(/'--- not available ---'/),1,1) !,ATTRIB,NATTRIB,2)   !y
  CALL WDIALOGPUTMENU(IDF_MENU5,(/'--- not available ---'/),1,1) !,ATTRIB,NATTRIB,3)   !z
  CALL WDIALOGPUTMENU(IDF_MENU6,(/'--- not available ---'/),1,1) !,ATTRIB,NATTRIB,3)   !z
  CALL WDIALOGPUTMENU(IDF_MENU7,(/'--- not available ---'/),1,1) !,ATTRIB,NATTRIB,3)   !z
  CALL WDIALOGFIELDSTATE(IDF_MENU3,3)
  CALL WDIALOGFIELDSTATE(IDF_MENU4,3)
  CALL WDIALOGFIELDSTATE(IDF_MENU5,3)
  CALL WDIALOGFIELDSTATE(IDF_MENU6,3)
  CALL WDIALOGFIELDSTATE(IDF_MENU7,3)
  CALL WDIALOGFIELDSTATE(IDF_LABEL2,3)
  CALL WDIALOGFIELDSTATE(IDF_LABEL3,3)
  CALL WDIALOGFIELDSTATE(IDF_LABEL4,3)
  CALL WDIALOGFIELDSTATE(IDF_LABEL5,3)
  CALL WDIALOGFIELDSTATE(IDF_LABEL6,3)
  CALL WDIALOGFIELDSTATE(IDF_LABEL10,3)
  CALL WDIALOGFIELDSTATE(IDF_CHECK3,3)
  CALL WDIALOGFIELDSTATE(IDF_CHECK5,3)
  CALL WDIALOGFIELDSTATE(IDF_CHECK6,3)
  CALL WDIALOGFIELDSTATE(IDF_INTEGER1,3)
  CALL WDIALOGFIELDSTATE(IDF_INTEGER2,3)
 ENDIF

 CALL SETTINGS_FIELDS(LGEN.OR.LISG)

 CALL WDIALOGSHOW(-1,-1,0,2)

 DO WHILE(.TRUE.)
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)

   CASE(FIELDCHANGED)
    CALL WDIALOGSELECT(MESSAGE%WIN)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (IDF_CHECK3,IDF_CHECK5,IDF_RADIO1,IDF_RADIO2,IDF_CHECK6)
      CALL SETTINGS_FIELDS(LGEN.OR.LISG)
    END SELECT
    SELECT CASE (MESSAGE%VALUE2)
    END SELECT

   CASE(PUSHBUTTON)
    CALL WDIALOGSELECT(MESSAGE%WIN)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (IDF_BUTTON6)
      IRGB=MP(IPLOT)%SCOLOR
      CALL WSELECTCOLOUR(IRGB)
      IF(WINFODIALOG(4).EQ.1)THEN
       MP(IPLOT)%SCOLOR=IRGB
       CALL WDIALOGCOLOUR(IDF_STRING1,MP(IPLOT)%SCOLOR,MP(IPLOT)%SCOLOR)
      ENDIF
     CASE (ID_SYMBOLS)
      CALL SETTINGS_IPFSYMBOLS(IPLOT,LIPF,LIFF,LISG,LGEN)
      CALL WDIALOGSELECT(ID_DCONFIGURE)
      CALL WDIALOGCOLOUR(IDF_STRING1,MP(IPLOT)%SCOLOR,MP(IPLOT)%SCOLOR)      
     CASE (ID_LABELING)
      IF(LIPF)CALL IMOD3D_LABELS(1,IPLOT)
      IF(LGEN)CALL GENLABELSDEFINE(IPLOT,ATTRIB,SIZE(ATTRIB))
     CASE (IDOK,IDCANCEL)
      !## fadeout
      CALL WDIALOGGETCHECKBOX(IDF_CHECK3,MP(IPLOT)%FADEOUT)
      IF(MP(IPLOT)%FADEOUT.EQ.1)CALL WDIALOGGETINTEGER(IDF_INTEGER1,MP(IPLOT)%IDFI)
      !## colouring
      CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,MP(IPLOT)%ILEG)
      MP(IPLOT)%ILEG=MP(IPLOT)%ILEG-1
      IF(MP(IPLOT)%ILEG.EQ.1)CALL WDIALOGGETMENU(IDF_MENU1,MP(IPLOT)%IATTRIB)
      
      IF(LIPF)THEN
       CALL WDIALOGGETMENU(IDF_MENU3,MP(IPLOT)%XCOL)
       CALL WDIALOGGETMENU(IDF_MENU4,MP(IPLOT)%YCOL)
       CALL WDIALOGGETMENU(IDF_MENU5,MP(IPLOT)%ZCOL)
       MP(IPLOT)%Z2COL=MP(IPLOT)%ZCOL
       CALL WDIALOGGETCHECKBOX(IDF_CHECK6,I)
       IF(I.EQ.1)CALL WDIALOGGETMENU(IDF_MENU7,MP(IPLOT)%Z2COL)
       CALL WDIALOGGETCHECKBOX(IDF_CHECK5,MP(IPLOT)%HCOL)
       IF(MP(IPLOT)%HCOL.EQ.1)CALL WDIALOGGETMENU(IDF_MENU6,MP(IPLOT)%HCOL)
       CALL WDIALOGGETINTEGER(IDF_INTEGER2,I)
       WRITE(CAXES,'(I10)') I
       READ(CAXES,'(10I1)') MP(IPLOT)%IAXES(1:10)
      ENDIF
      !## added legend colouring, for the first time ... make legend (linear)
      IF(.NOT.LLEG.AND.MP(IPLOT)%ILEG.EQ.1)THEN
       !## draw map to get info abount points
       CALL IDFPLOT(1)
       !## legend legend entire region
       CALL LEG_CREATEINIT(ID_CDLL)
      ENDIF

      EXIT
      
     CASE (IDHELP)
      CALL IMODGETHELP('4.5.2','MMO.GO.Config')
    END SELECT
  END SELECT

 ENDDO

 IF(ALLOCATED(ATTRIB))DEALLOCATE(ATTRIB)

 CALL WDIALOGSELECT(ID_DCONFIGURE)
 CALL WDIALOGUNLOAD()

 END SUBROUTINE SETTINGS_MAIN

 !###======================================================================
 SUBROUTINE SETTINGS_FIELDS(LEX)
 !###======================================================================
 IMPLICIT NONE
 LOGICAL,INTENT(IN) :: LEX
 INTEGER :: I,J

 !## colouring
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,I)
 J=1
 IF(I.EQ.1)J=2
 CALL WDIALOGFIELDSTATE(IDF_BUTTON6,I)
 CALL WDIALOGFIELDSTATE(IDF_STRING1,I)
 CALL WDIALOGFIELDSTATE(IDF_MENU1,J)

 IF(LEX)RETURN

 !## second z-coordinate
 CALL WDIALOGGETCHECKBOX(IDF_CHECK6,I)
 CALL WDIALOGFIELDSTATE(IDF_MENU7,I)

 !## sigth depth
 CALL WDIALOGGETCHECKBOX(IDF_CHECK3,I)
 CALL WDIALOGFIELDSTATE(IDF_INTEGER1,I)
 CALL WDIALOGFIELDSTATE(IDF_LABEL5,I)

! !## tic/marks
! CALL WDIALOGGETCHECKBOX(IDF_CHECK4,I)
! CALL WDIALOGFIELDSTATE(IDF_BUTTON5,I)

 !## highlight
 CALL WDIALOGGETCHECKBOX(IDF_CHECK5,I)
 CALL WDIALOGFIELDSTATE(IDF_MENU6,I)

 END SUBROUTINE SETTINGS_FIELDS

!###======================================================================
SUBROUTINE SETTINGS_IPFSYMBOLS(IPLOT,LIPF,LIFF,LISG,LGEN)
!###======================================================================
IMPLICIT NONE
INTEGER,INTENT(IN) :: IPLOT
LOGICAL,INTENT(IN) :: LIPF,LIFF,LISG,LGEN
TYPE(WIN_MESSAGE) :: MESSAGE
INTEGER :: ITYPE,I,IRGB,JRGB
CHARACTER(LEN=2),ALLOCATABLE,DIMENSION(:) :: CLIST

CALL WDIALOGLOAD(ID_DSYMBOLS,ID_DSYMBOLS)

IF(LIPF)THEN
 CALL WDIALOGPUTSTRING(IDF_GROUP1,'Marker Symbol')
 IF(ALLOCATED(CLIST))DEALLOCATE(CLIST)
 ALLOCATE(CLIST(40))
 DO I=1,40
  WRITE(CLIST(I),'(I2)') I
 END DO
 ITYPE=MP(IPLOT)%SYMBOL
 IF(ITYPE.LE.0.OR.ITYPE.GT.40)ITYPE=14
 CALL WDIALOGPUTMENU(IDF_MENU1,CLIST,40,ITYPE)
ENDIF

IF(LIFF.OR.LISG.OR.LGEN)THEN
 CALL WDIALOGPUTSTRING(IDF_GROUP1,'Line Symbol')
 IF(ALLOCATED(CLIST))DEALLOCATE(CLIST)
 ALLOCATE(CLIST(8))
 DO I=1,8
  WRITE(CLIST(I),'(I2)') I-1
 END DO
 ITYPE=MP(IPLOT)%SYMBOL+1
 IF(ITYPE.LE.0.OR.ITYPE.GT.8)ITYPE=1
 CALL WDIALOGPUTMENU(IDF_MENU1,CLIST,8,ITYPE)
ENDIF
CALL WDIALOGPUTINTEGER(IDF_INTEGER1,MP(IPLOT)%THICKNESS)

IF(ALLOCATED(CLIST))DEALLOCATE(CLIST)

IF(LGEN)THEN
 CALL WDIALOGFIELDSTATE(IDF_CHECK1,1)
 CALL WDIALOGPUTCHECKBOX(IDF_CHECK1,MP(IPLOT)%PRFTYPE)
ELSE
 CALL WDIALOGFIELDSTATE(IDF_CHECK1,2)
 CALL WDIALOGPUTCHECKBOX(IDF_CHECK1,0)
ENDIF

IF(LISG)THEN
 CALL WDIALOGFIELDSTATE(ID_LEGEND,0)
ENDIF
IRGB=MP(IPLOT)%SCOLOR

CALL WDIALOGSHOW(-1,-1,0,3)
CALL SETTINGS_SYMBOLDRAW(LIFF,LIPF,LISG,LGEN,IRGB)

DO

 CALL WMESSAGE(ITYPE, MESSAGE)

 SELECT CASE (ITYPE)

  CASE (FIELDCHANGED)

    SELECT CASE (MESSAGE%VALUE2)
     CASE (IDF_MENU1,IDF_INTEGER1)
      CALL SETTINGS_SYMBOLDRAW(LIFF,LIPF,LISG,LGEN,IRGB)
    END SELECT

  CASE(PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)

    CASE (ID_LEGEND)
     CALL LEG_MAIN(0)

    CASE (ID_COLOR)
     IRGB=MP(IPLOT)%SCOLOR
     JRGB=IRGB
     CALL WSELECTCOLOUR(IRGB)
     IF(WINFODIALOG(4).EQ.1)THEN
      MP(IPLOT)%SCOLOR=IRGB
     ELSE
      IRGB=JRGB
     ENDIF
     CALL SETTINGS_SYMBOLDRAW(LIFF,LIPF,LISG,LGEN,IRGB)

    CASE (IDOK)
     CALL WDIALOGGETMENU(IDF_MENU1,MP(IPLOT)%SYMBOL)
     IF(LIFF.OR.LISG.OR.LGEN)THEN
      MP(IPLOT)%SYMBOL=MP(IPLOT)%SYMBOL-1
     ENDIF
     CALL WDIALOGGETINTEGER(IDF_INTEGER1,MP(IPLOT)%THICKNESS)
     CALL WDIALOGGETCHECKBOX(IDF_CHECK1,MP(IPLOT)%PRFTYPE)
     EXIT
    CASE (IDCANCEL)
     EXIT
   END SELECT

 END SELECT
ENDDO

CALL WDIALOGSELECT(ID_DSYMBOLS)
CALL WDIALOGUNLOAD()

IF(MESSAGE%VALUE1.EQ.IDOK)CALL IDFPLOTFAST(0)

END SUBROUTINE SETTINGS_IPFSYMBOLS

!###======================================================================
SUBROUTINE SETTINGS_GENSYMBOLS()
!###======================================================================
IMPLICIT NONE
TYPE(WIN_MESSAGE)                :: MESSAGE
INTEGER                          :: ITYPE,I,IRGB,JRGB,IGEN
CHARACTER(LEN=2),ALLOCATABLE,DIMENSION(:) :: CLIST
LOGICAL :: LIPF,LGEN

CALL WDIALOGLOAD(ID_DSYMBOLS,ID_DSYMBOLS)

DO IGEN=1,MXGEN; IF(GEN(IGEN)%ISEL)EXIT; ENDDO

LIPF=.FALSE.; LGEN=.FALSE.
IF(GEN(IGEN)%ITYPE.EQ.2.OR.GEN(IGEN)%ITYPE.EQ.-1)THEN
 LIPF=.TRUE.
 CALL WDIALOGPUTSTRING(IDF_GROUP1,'Marker Symbol')
 IF(ALLOCATED(CLIST))DEALLOCATE(CLIST)
 ALLOCATE(CLIST(40))
 DO I=1,40
  WRITE(CLIST(I),'(I2)') I
 END DO
 ITYPE=GEN(IGEN)%SYMBOL
 IF(ITYPE.LE.0.OR.ITYPE.GT.40)ITYPE=14
 CALL WDIALOGPUTMENU(IDF_MENU1,CLIST,40,ITYPE)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER1,GEN(IGEN)%THICKNESS)
 CALL WDIALOGFIELDSTATE(IDF_CHECK1,1)
 CALL WDIALOGPUTSTRING(IDF_LABEL3,'Marker Size')
ELSEIF(GEN(IGEN)%ITYPE.EQ.1.OR.GEN(IGEN)%ITYPE.EQ.3)THEN !gen/shp
 LGEN=.TRUE.
 CALL WDIALOGPUTSTRING(IDF_GROUP1,'Line Symbol')
 IF(ALLOCATED(CLIST))DEALLOCATE(CLIST)
 ALLOCATE(CLIST(8))
 DO I=1,8
  WRITE(CLIST(I),'(I2)') I-1
 END DO
 ITYPE=GEN(IGEN)%SYMBOL+1
 IF(ITYPE.LE.0.OR.ITYPE.GT.8)ITYPE=1
 CALL WDIALOGPUTMENU(IDF_MENU1,CLIST,8,ITYPE)
 IF(ALLOCATED(CLIST))DEALLOCATE(CLIST)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER1,GEN(IGEN)%THICKNESS)
 CALL WDIALOGFIELDSTATE(IDF_CHECK1,1)
 CALL WDIALOGPUTSTRING(IDF_LABEL3,'Thickness')
ENDIF

CALL WDIALOGFIELDSTATE(ID_LEGEND,2)
CALL WDIALOGFIELDSTATE(IDF_CHECK1,3)

CALL WDIALOGSHOW(-1,-1,0,3)

IRGB=GEN(IGEN)%RGB
CALL SETTINGS_SYMBOLDRAW(.FALSE.,LIPF,.FALSE.,LGEN,IRGB)

DO

 CALL WMESSAGE(ITYPE, MESSAGE)

 SELECT CASE (ITYPE)

  CASE (FIELDCHANGED)

    SELECT CASE (MESSAGE%VALUE2)
     CASE (IDF_INTEGER1,IDF_MENU1)
      CALL SETTINGS_SYMBOLDRAW(.FALSE.,LIPF,.FALSE.,LGEN,IRGB)

    END SELECT

  CASE(PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
    CASE (ID_COLOR)
     JRGB=IRGB
     CALL WSELECTCOLOUR(IRGB)
     IF(WINFODIALOG(4).NE.1)IRGB=JRGB
     CALL SETTINGS_SYMBOLDRAW(.FALSE.,LIPF,.FALSE.,LGEN,IRGB)
    CASE (IDOK)
     CALL WDIALOGGETMENU(IDF_MENU1,GEN(IGEN)%SYMBOL)
     IF(LGEN)GEN(IGEN)%SYMBOL=GEN(IGEN)%SYMBOL-1
     GEN(IGEN)%RGB=IRGB
     CALL WDIALOGGETINTEGER(IDF_INTEGER1,GEN(IGEN)%THICKNESS)
     EXIT
    CASE (IDCANCEL)
     EXIT
   END SELECT

 END SELECT
ENDDO

CALL WDIALOGSELECT(ID_DSYMBOLS)
CALL WDIALOGUNLOAD()

IF(MESSAGE%VALUE1.EQ.IDOK)CALL IDFPLOTFAST(0)

END SUBROUTINE SETTINGS_GENSYMBOLS

!###======================================================================
SUBROUTINE SETTINGS_SYMBOLDRAW(LIFF,LIPF,LISG,LGEN,IRGB)
!###======================================================================
IMPLICIT NONE
LOGICAL,INTENT(IN) :: LIFF,LIPF,LISG,LGEN
INTEGER,INTENT(IN) :: IRGB
INTEGER :: IMARKER,I

CALL WDIALOGSELECT(ID_DSYMBOLS)
CALL IGRPLOTMODE(MODECOPY)

CALL IGRSELECT(DRAWFIELD,IDF_PICTURE1)
CALL IGRCOLOURN(WRGB(255,255,255))
CALL IGRFILLPATTERN(SOLID)
CALL IGRRECTANGLE(0.0,0.0,1.0,1.0)
CALL IGRFILLPATTERN(OUTLINE)
CALL IGRCOLOURN(WRGB(0,0,0))
CALL IGRAREA(0.0,0.0,1.0,1.0)
CALL IGRUNITS(0.0,0.0,1.0,1.0)

CALL IGRCOLOURN(IRGB)

!marker
IF(LIPF)THEN
 CALL IGRLINETYPE(SOLIDLINE)
 CALL WDIALOGGETINTEGER(IDF_INTEGER1,I)
 CALL WGRTEXTFONT(WIDTH=(REAL(I)*4.0)/75.,HEIGHT=(REAL(I)*2.0)/25.)
 CALL WDIALOGGETMENU(IDF_MENU1,IMARKER)
 CALL IGRMARKER(0.5,0.5,IMARKER)
ENDIF

!lines
IF(LIFF.OR.LISG.OR.LGEN)THEN
 CALL WDIALOGGETMENU(IDF_MENU1,IMARKER)
 CALL IGRLINETYPE(IMARKER-1)
 CALL WDIALOGGETINTEGER(IDF_INTEGER1,I)
 CALL IGRLINEWIDTH(I)
 CALL IGRJOIN(0.05,0.05,0.95,0.3)
 CALL IGRJOIN(0.95,0.3,0.05,0.6)
 CALL IGRJOIN(0.05,0.6,0.95,0.95)
ENDIF

END SUBROUTINE SETTINGS_SYMBOLDRAW

END MODULE MOD_SETTINGS
