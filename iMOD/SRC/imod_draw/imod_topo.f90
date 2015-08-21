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
!!
MODULE MOD_TOPO

USE WINTERACTER
USE RESOURCE
USE BMPVAR
USE IMODVAR, ONLY : OPENDIR
USE MOD_UTL, ONLY : UTL_GETUNIT,UTL_WSELECTFILE,ITOS,RTOS,UTL_MESSAGEHANDLE,UTL_CREATEDIR,UTL_LOADIMAGE
USE MOD_OSD, ONLY : OSD_OPEN
USE MODPLOT, ONLY : MPW,ZM
USE MOD_PREF_PAR, ONLY : PREFVAL

CONTAINS

 !###======================================================================
 SUBROUTINE TOPO1MAIN()
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITYPE,I

 CALL WDIALOGLOAD(ID_DADDTOPO,ID_DADDTOPO)
 IF(NBMP.GT.0)THEN
  CALL WDIALOGFIELDSTATE(IDF_MENU1,1)
  CALL WDIALOGPUTMENU(IDF_MENU1,BMP%BMPFNAME,NBMP,BMP%IACT)
 ELSE
  CALL WDIALOGCLEARFIELD(IDF_MENU1)
  CALL WDIALOGFIELDSTATE(IDF_MENU1,2)
 ENDIF
 
 CALL TOPO1FIELD()

 CALL WDIALOGFIELDOPTIONS(IDF_REAL1,EDITFIELDCHANGED,1)
 CALL WDIALOGFIELDOPTIONS(IDF_REAL2,EDITFIELDCHANGED,1)
 CALL WDIALOGFIELDOPTIONS(IDF_REAL5,EDITFIELDCHANGED,1)

 CALL WDIALOGSHOW(-1,-1,0,3)

 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  CALL WDIALOGSELECT(MESSAGE%WIN)
  SELECT CASE (ITYPE)

   CASE (FIELDCHANGED)
    !## moved from
    SELECT CASE (MESSAGE%VALUE1)
     CASE (IDF_REAL1,IDF_REAL2,IDF_REAL5)
      CALL TOPO2FIELD()
      CALL TOPO1FIELD()
    END SELECT
    !## moved to
    SELECT CASE (MESSAGE%VALUE2)
     CASE (IDF_MENU1)
      CALL TOPO1FIELD()
    END SELECT

   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     !## open
     CASE (IDF_BUTTON4)
      CALL TOPO1OPENBMP(1)
      CALL TOPO1FIELD()
     !## delete
     CASE (IDF_BUTTON5)
      CALL TOPO1DELETEBMP()
      CALL TOPO1FIELD()
     !## accept
     CASE (IDCANCEL)
      EXIT
     CASE (IDOK)
      DO I=1,NBMP
       CALL TOPO1LOADBMP(BMP(I)%IBITMAP,BMP(I)%BMPFNAME,BMP(I)%NCOL,BMP(I)%NROW)
       IF(BMP(I)%IBITMAP.EQ.0)EXIT
      ENDDO
      !## okay, everything read, let's adjust the zoomsettings
      IF(I.GT.NBMP.AND.NBMP.GT.0)THEN
       MPW%XMIN= 10.0E10; MPW%XMAX=-10.0E10
       MPW%YMIN= 10.0E10; MPW%YMAX=-10.0E10
       DO I=1,NBMP
        MPW%XMIN=MIN(MPW%XMIN,BMP(I)%XMIN)
        MPW%XMAX=MAX(MPW%XMAX,BMP(I)%XMAX)
        MPW%YMIN=MIN(MPW%YMIN,BMP(I)%YMIN)
        MPW%YMAX=MAX(MPW%YMAX,BMP(I)%YMAX)
       ENDDO
      ENDIF
      IF(I.GT.NBMP)THEN
       CALL IDFSTOREZOOMEXTENT()
       EXIT
      ENDIF
     CASE(ID_CLOSE)
      EXIT
     CASE(IDHELP)
      CALL IMODGETHELP('3.3.3','VMO.AddTopo') 
    END SELECT
  END SELECT
 ENDDO
 CALL WDIALOGUNLOAD()
 !## make sure to turn check off first
 CALL WINDOWSELECT(0); CALL WMENUSETSTATE(ID_TOPOGRAPHY,2,0)
 CALL TOPOINIT()

 !## zoomprevious and zoomnext settings
 CALL WINDOWSELECT(0)
 I=0; IF(ZM%IZOOM.GT.1)I=1
 CALL WMENUSETSTATE(ID_ZOOMPREVIOUS,1,I)
 I=0; IF(ZM%IZOOM.LT.ZM%NZOOM)I=1
 CALL WMENUSETSTATE(ID_ZOOMNEXT,1,I)

 END SUBROUTINE TOPO1MAIN

!###======================================================================
 SUBROUTINE TOPO1UPDATEMANAGER()
 !###======================================================================
 IMPLICIT NONE
 LOGICAL :: LEX
 
 LEX=.FALSE.
 IF(LEN_TRIM(PREFVAL(2)).GT.0)INQUIRE(FILE=TRIM(PREFVAL(2)),EXIST=LEX)
 IF(SUM(BMP(1:NBMP)%IACT).GT.0)LEX=.TRUE.

 IF(LEX)THEN
  IF(WMENUGETSTATE(ID_TOPOGRAPHY,1).EQ.0)THEN
   CALL WMENUSETSTATE(ID_TOPOGRAPHY,1,1)
   CALL WMENUSETSTATE(ID_TOPTRANSPARACY,1,1)
  ENDIF
 ELSE
  IF(WMENUGETSTATE(ID_TOPOGRAPHY,1).EQ.1)THEN
   CALL WMENUSETSTATE(ID_TOPOGRAPHY,2,0)
   CALL WMENUSETSTATE(ID_TOPOGRAPHY,1,0)
   CALL WMENUSETSTATE(ID_TOPTRANSPARACY,1,0)
  ENDIF
 ENDIF

 END SUBROUTINE TOPO1UPDATEMANAGER
 
 !###======================================================================
 SUBROUTINE TOPO1OPENBMP(OPENDIALOG)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: OPENDIALOG
 INTEGER :: I,IU
 INTEGER,ALLOCATABLE,DIMENSION(:) :: INFO
 CHARACTER(LEN=4) :: EXT
 LOGICAL :: LEX
 REAL :: DX,DY,OR1,OR2

 IF(OPENDIALOG.EQ.1)THEN !## when .eq. '0', images can be loaded in the background without opening a dialog
  BMP(NBMP+1)%BMPFNAME=TRIM(OPENDIR)
  IF(.NOT.UTL_WSELECTFILE('All Known Files|*.bmp;*.png|BitMap (*.bmp)|Portable Network Graphic image (*.png)|',&
                   LOADDIALOG+MUSTEXIST+PROMPTON+DIRCHANGE+APPENDEXT+MULTIFILE,BMP(NBMP+1)%BMPFNAME,&
                   'Load Background BitMap (*.bmp;*.png)'))RETURN
 END IF                 

 I=INDEXNOCASE(BMP(NBMP+1)%BMPFNAME,'\',.TRUE.)
 OPENDIR=BMP(NBMP+1)%BMPFNAME(:I-1)

 CALL IUPPERCASE(BMP(NBMP+1)%BMPFNAME)
 IF(ALLOCATED(INFO))DEALLOCATE(INFO)
 ALLOCATE(INFO(6))
 CALL IGRFILEINFO(BMP(NBMP+1)%BMPFNAME,INFO,6)
 BMP(NBMP+1)%ITYPE=INFO(1)
 SELECT CASE (BMP(NBMP+1)%ITYPE)
  !## bmp
  CASE (1)
   BMP(NBMP+1)%NCOL =INFO(2)!INFO(2) Image width in pixels.
   BMP(NBMP+1)%NROW =INFO(3)!INFO(3) Image height in pixels.
   BMP(NBMP+1)%NCLR =INFO(4)!INFO(4) Number of colours.
   BMP(NBMP+1)%COMPR=INFO(5)!INFO(5) Is file compressed ? 0 = no , 1 = yes.
   BMP(NBMP+1)%CDEPT=INFO(6)!INFO(6) Colour depth in bits-per-pixel (1-32)
   EXT='BMPW'
  !## png
  CASE (15)
   BMP(NBMP+1)%NCOL =INFO(2)!INFO(2) Image width in pixels.
   BMP(NBMP+1)%NROW =INFO(3)!INFO(3) Image height in pixels.
   BMP(NBMP+1)%NCLR =INFO(4)!INFO(4) Number of colours.
   BMP(NBMP+1)%COMPR=INFO(5)!INFO(5) Interlaced image (0=no 1=yes)
   BMP(NBMP+1)%CDEPT=INFO(6)!INFO(6) Colour depth in bits-per-pixel (1-48)
   EXT='PNGW'
  CASE DEFAULT
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'No suitable file-format found','Error')
 END SELECT
 IF(ALLOCATED(INFO))DEALLOCATE(INFO)

 IF(BMP(NBMP+1)%ITYPE.LE.0)RETURN

 I=INDEXNOCASE(BMP(NBMP+1)%BMPFNAME,'.',.TRUE.)

 !## search world-file
 INQUIRE(FILE=BMP(NBMP+1)%BMPFNAME(:I-1)//'.'//TRIM(EXT),EXIST=LEX)
 IF(LEX)THEN
  IU=UTL_GETUNIT()
  CALL OSD_OPEN(IU,FILE=BMP(NBMP+1)%BMPFNAME(:I-1)//'.'//TRIM(EXT),STATUS='OLD',ACTION='READ,DENYWRITE')
  READ(IU,*) DX
  READ(IU,*) OR1
  READ(IU,*) OR2
  READ(IU,*) DY
  IF(OR1.EQ.0.0.AND.OR2.EQ.0.0)THEN
   BMP(NBMP+1)%DX=DX; BMP(NBMP+1)%DY=ABS(DY)
   READ(IU,*) BMP(NBMP+1)%XMIN
   READ(IU,*) BMP(NBMP+1)%YMAX
   BMP(NBMP+1)%XMAX=BMP(NBMP+1)%XMIN+(BMP(NBMP+1)%NCOL*BMP(NBMP+1)%DX)
   BMP(NBMP+1)%YMIN=BMP(NBMP+1)%YMAX-(BMP(NBMP+1)%NROW*BMP(NBMP+1)%DY)
!   BMP(NBMP+1)%YMIN=BMP(NBMP+1)%YMAX-(BMP(NBMP+1)%NROW*BMP(NBMP+1)%DXY)
!            14.250000000000000
!             0.000000000000000
!             0.000000000000000
!           -14.250000000000000
!        193899.750000000000000
!       5543948.000000000000000
  ELSE
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not read a georeference file in which second and third line are not'//&
    ' zero'//CHAR(13)//'This BMP/PNG will probably be positioned wrongly!','Error')
   LEX=.FALSE.
  ENDIF
  CLOSE(IU)
 !## initialize parameters
 ELSE
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not open/read the selected file'//CHAR(13)// &
      BMP(NBMP+1)%BMPFNAME(:I-1)//'.'//TRIM(EXT),'Error')
  RETURN
 ENDIF

 NBMP=NBMP+1
 CALL WDIALOGSELECT(ID_DADDTOPO)
 CALL WDIALOGFIELDSTATE(IDF_MENU1,1)
 BMP(NBMP)%IACT=1
 CALL WDIALOGPUTMENU(IDF_MENU1,BMP%BMPFNAME,NBMP,BMP%IACT)
 !## no more bitmaps to be added
 IF(NBMP.GT.MXBMP)CALL WDIALOGFIELDSTATE(IDF_BUTTON4,2)

 END SUBROUTINE TOPO1OPENBMP

 !###======================================================================
 SUBROUTINE TOPO1LOADBMP(IBITMAP,FNAME,NCOL,NROW)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: IBITMAP
 INTEGER,INTENT(IN) :: NCOL,NROW
 CHARACTER(LEN=256),INTENT(IN) :: FNAME
 INTEGER :: IW,IH
 
 IW=NCOL !MIN(WINFOBITMAP(MPW%IBITMAP,BITMAPWIDTH) ,NCOL)
 IH=NROW !MIN(WINFOBITMAP(MPW%IBITMAP,BITMAPHEIGHT),NROW)
 CALL WBITMAPCREATE(IBITMAP,IW,IH)
 IF(IBITMAP.EQ.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not create memory for image:'//CHAR(13)//TRIM(FNAME),'Error')
  RETURN
 ENDIF
 CALL WBITMAPSTRETCHMODE(STRETCHHALFTONE)
 CALL WBITMAPLOAD(IBITMAP,FNAME,1)
 CALL WBITMAPSTRETCHMODE(0)

 END SUBROUTINE TOPO1LOADBMP
 
 !###======================================================================
 SUBROUTINE TOPO1DELETEBMP()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J

 CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Are you sure to remove the selected files from the menu?','Question')
 IF(WINFODIALOG(4).NE.1)RETURN

 CALL WDIALOGGETMENU(IDF_MENU1,BMP%IACT)
 J=0
 DO I=1,NBMP
  IF(BMP(I)%IACT.EQ.0)THEN
   J=J+1
   BMP(J)=BMP(I)
  ENDIF
 END DO
 NBMP=J

 CALL WDIALOGSELECT(ID_DADDTOPO)

 BMP%IACT=0
 IF(NBMP.GT.0)THEN
  CALL WDIALOGFIELDSTATE(IDF_MENU1,1)
  CALL WDIALOGPUTMENU(IDF_MENU1,BMP%BMPFNAME,NBMP,BMP%IACT)
 ELSE
  CALL WDIALOGCLEARFIELD(IDF_MENU1)
  CALL WDIALOGFIELDSTATE(IDF_MENU1,2)
 ENDIF
 IF(NBMP.LE.MXBMP)CALL WDIALOGFIELDSTATE(IDF_BUTTON4,1)

 END SUBROUTINE TOPO1DELETEBMP

 !###======================================================================
 SUBROUTINE TOPO1FIELD()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 CALL WDIALOGSELECT(ID_DADDTOPO)
 CALL WDIALOGGETMENU(IDF_MENU1,BMP%IACT)

 !## one selected
 I=0; IF(SUM(BMP%IACT).EQ.1)I=1

 !## coordinates editable (lower-left corner only)
 CALL WDIALOGFIELDSTATE(IDF_REAL1,I)
 CALL WDIALOGFIELDSTATE(IDF_REAL2,I)
 CALL WDIALOGFIELDSTATE(IDF_REAL5,I)

 !## nothing selected, clear fields
 IF(I.EQ.0)THEN

  CALL WDIALOGCLEARFIELD(IDF_INTEGER1)
  CALL WDIALOGCLEARFIELD(IDF_INTEGER2)
  CALL WDIALOGCLEARFIELD(IDF_REAL1)
  CALL WDIALOGCLEARFIELD(IDF_REAL2)
  CALL WDIALOGCLEARFIELD(IDF_REAL3)
  CALL WDIALOGCLEARFIELD(IDF_REAL4)
  CALL WDIALOGCLEARFIELD(IDF_REAL5)
  CALL WDIALOGCLEARFIELD(IDF_REAL6)
  CALL WDIALOGPUTINTEGER(IDF_INTEGER3,0)
  CALL WDIALOGFIELDSTATE(IDF_BUTTON5,0)

 !## fill in coordinates of activated bmp's
 ELSE
  !## delete bmp from list becomes activated
  CALL WDIALOGFIELDSTATE(IDF_BUTTON5,1)
  DO I=1,NBMP
   IF(BMP(I)%IACT.EQ.1)THEN
    CALL WDIALOGPUTINTEGER(IDF_INTEGER1,BMP(I)%NCOL)
    CALL WDIALOGPUTINTEGER(IDF_INTEGER2,BMP(I)%NROW)
    CALL WDIALOGPUTREAL(IDF_REAL1,BMP(I)%XMIN/1000.0,'(F10.2)')
    CALL WDIALOGPUTREAL(IDF_REAL2,BMP(I)%YMIN/1000.0,'(F10.2)')
    CALL WDIALOGPUTREAL(IDF_REAL4,BMP(I)%XMAX/1000.0,'(F10.2)')
    CALL WDIALOGPUTREAL(IDF_REAL3,BMP(I)%YMAX/1000.0,'(F10.2)')
    CALL WDIALOGPUTREAL(IDF_REAL5,BMP(I)%DX,'(F10.2)')
    CALL WDIALOGPUTREAL(IDF_REAL6,BMP(I)%DY,'(F10.2)')
    EXIT
   ENDIF
  ENDDO
 ENDIF

 END SUBROUTINE TOPO1FIELD

 !###======================================================================
 SUBROUTINE TOPO2FIELD()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 CALL WDIALOGSELECT(ID_DADDTOPO)
 CALL WDIALOGGETMENU(IDF_MENU1,BMP%IACT)
 DO I=1,SIZE(BMP); IF(BMP(I)%IACT.EQ.1)EXIT; ENDDO
 
 CALL WDIALOGGETREAL(IDF_REAL1,BMP(I)%XMIN)
 CALL WDIALOGGETREAL(IDF_REAL2,BMP(I)%YMIN)
 CALL WDIALOGGETREAL(IDF_REAL5,BMP(I)%DX)
 
 BMP(I)%DY=BMP(I)%DX

 BMP(I)%XMIN=BMP(I)%XMIN*1000.0
 BMP(I)%YMIN=BMP(I)%YMIN*1000.0
 BMP(I)%XMAX=BMP(I)%XMIN+BMP(I)%NCOL*BMP(I)%DX
 BMP(I)%YMAX=BMP(I)%YMIN+BMP(I)%NROW*BMP(I)%DY
 
 END SUBROUTINE TOPO2FIELD

 !###======================================================================
 SUBROUTINE TOPOINIT()
 !###======================================================================
 IMPLICIT NONE

 CALL WINDOWSELECT(0)
 IF(WMENUGETSTATE(ID_TOPOGRAPHY,2).EQ.0)THEN
  CALL WMENUSETSTATE(ID_TOPOGRAPHY,2,1)
 ELSE
  CALL WMENUSETSTATE(ID_TOPOGRAPHY,2,0)
 ENDIF

 CALL IDFPLOTFAST(0)

 END SUBROUTINE TOPOINIT

 !###======================================================================
 SUBROUTINE TOPO1DRAW(IWIN,WINXMIN,WINYMIN,WINXMAX,WINYMAX,WINIBITMAP)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,PARAMETER :: SSIZE=2000 !## bmp scale size
 REAL,INTENT(IN) :: WINXMIN,WINYMIN,WINXMAX,WINYMAX
 INTEGER,INTENT(IN) :: IWIN,WINIBITMAP
 LOGICAL :: LEX
 REAL :: XMIN,YMIN,XMAX,YMAX,DXBMP,DX1,DX2,DX
 INTEGER :: NROW,NCOL,IU,IOS,I,IBMP
 INTEGER :: IX1,IX2,IY1,IY2,IHANDLE
 CHARACTER(LEN=256) :: BMPFNAME

 LOGICAL :: LBMP

 CALL TOPO1UPDATEMANAGER() 

 LBMP=.FALSE.
 IF(NBMP.GT.0)THEN
  IF(SUM(BMP(1:NBMP)%IACT).GT.0)LBMP=.TRUE.
 ENDIF

 IF(.NOT.LBMP)THEN

  !## this can happen whenever a butmap is deselected
  IF(WMENUGETSTATE(ID_TOPOGRAPHY,1).EQ.0)RETURN

  I =INDEXNOCASE(PREFVAL(2),'\',.TRUE.)
  IU=UTL_GETUNIT()
  CALL OSD_OPEN(IU,FILE=PREFVAL(2),FORM='FORMATTED',ACTION='READ,DENYWRITE')
  DX=WINXMAX-WINXMIN
  BMPFNAME=''
  DO WHILE(.TRUE.)
   READ(IU,*,IOSTAT=IOS) DX1,DX2,BMPFNAME
   DX1=DX1*1000.0
   DX2=DX2*1000.0
   IF(IOS.NE.0)EXIT
   IF(DX.GT.DX1.AND.DX.LE.DX2)EXIT
  ENDDO
  CLOSE(IU)
  IF(LEN_TRIM(BMPFNAME).EQ.0)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD has changed the organization of the TOP25 slightly!'//CHAR(13)// &
     'You should specify yourself at with zoomlevel different bmp"s will be used'//CHAR(13)// &
     'Edit the crd-file specified by the keyword TOP25 as follows:'//CHAR(13)//CHAR(13)// &
     '0,10,bmpcrd02.txt'//CHAR(13)// &
     '10,25,bmpcrd05.txt'//CHAR(13)// &
     '25,50,bmpcrd10.txt'//CHAR(13)// &
     '50,75,bmpcrd25.txt'//CHAR(13)// &
     '75,1000,bmpcrd50.txt'//CHAR(13)//CHAR(13)// &
     'See Help for more information.','Error')
   RETURN
  ENDIF
  BMPFNAME=PREFVAL(2)(:I)//TRIM(BMPFNAME)

 ENDIF

 CALL WINDOWSELECT(IWIN)
 IF(WMENUGETSTATE(ID_TOPTRANSPARACY,2).EQ.1)CALL WBITMAPPLOTMODE(MODEAND)

 IF(.NOT.LBMP)CALL OSD_OPEN(IU,FILE=BMPFNAME,FORM='FORMATTED',ACTION='READ,DENYWRITE')

 I=INDEXNOCASE(PREFVAL(2),'\',.TRUE.)
 IBMP=0
 DO

  IBMP=IBMP+1

  !## background images
  IF(.NOT.LBMP)THEN
  
   READ(IU,*,IOSTAT=IOS) XMIN,YMIN,XMAX,YMAX,NCOL,NROW,DXBMP,BMPFNAME
   IF(IOS.NE.0)EXIT; BMPFNAME=PREFVAL(2)(:I)//TRIM(BMPFNAME)
  
  !## entered background images
  ELSE

   IF(IBMP.GT.NBMP)EXIT; IF(BMP(IBMP)%IACT.EQ.0)CYCLE
   XMIN=BMP(IBMP)%XMIN; XMAX=BMP(IBMP)%XMAX
   YMIN=BMP(IBMP)%YMIN; YMAX=BMP(IBMP)%YMAX
   IHANDLE=BMP(IBMP)%IBITMAP   
   !## reread bitmap again
   IF(IHANDLE.EQ.0)CALL TOPO1LOADBMP(IHANDLE,BMP(IBMP)%BMPFNAME,BMP(IBMP)%NCOL,BMP(IBMP)%NROW)
   BMP(IBMP)%IBITMAP=IHANDLE
  ENDIF

  !## check whether current plot inside current plot-domain AND inside imod-box-limits
  IF(XMIN.LT.WINXMAX.AND.XMAX.GE.WINXMIN.AND.YMIN.LT.WINYMAX.AND.YMAX.GE.WINYMIN)THEN

   IF(.NOT.LBMP)CALL TOPO1LOADBMP(IHANDLE,BMPFNAME,NCOL,NROW)
   !## screen units
   CALL IGRUNITSTOPIXELS(XMIN,YMIN,IX1,IY1,IORIGIN=1)
   CALL IGRUNITSTOPIXELS(XMAX,YMAX,IX2,IY2,IORIGIN=1)
   CALL WBITMAPSTRETCHMODE(STRETCHHALFTONE)
   CALL WBITMAPPUT(IHANDLE,1,1,IX1,IY1,IX2,IY2)

   IF(.NOT.LBMP)CALL WBITMAPDESTROY(IHANDLE)
   
  ENDIF
 END DO

 CALL WINDOWSELECT(IWIN); IF(WMENUGETSTATE(ID_TOPTRANSPARACY,2).EQ.1)CALL WBITMAPPLOTMODE(MODECOPY)

 IF(.NOT.LBMP)CLOSE(IU)
 CALL WINDOWSELECT(IWIN); CALL WINDOWOUTSTATUSBAR(2,''); CALL WINDOWOUTSTATUSBAR(4,'')

 END SUBROUTINE TOPO1DRAW
 
 !###======================================================================
 SUBROUTINE TOPO1DRAW_TILING(BMPFNAME,OUTPUTFOLDER,IBATCH,MXS,NS)
 !###======================================================================
 USE MOD_IDF_PAR, ONLY : IDFOBJ
 USE MOD_IDF, ONLY : IDFWRITE
 IMPLICIT NONE 
 CHARACTER(LEN=*),INTENT(IN) :: BMPFNAME,OUTPUTFOLDER
 REAL,PARAMETER :: DXW=25.0 !## size of bitmaps (km)
 INTEGER,INTENT(IN) :: IBATCH,MXS,NS !## max number of rows/columns/number of samplings
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IBMPDATA
 INTEGER :: I,II,J,JJ,K,KK,IRED,IGREEN,IBLUE,JRED,JGREEN,JBLUE,NC,NR,IU,NROW,NCOL,IOS,IS,JU,IC,IC1,IC2,IR,IR1,IR2,BNC,BNR,N,DS
 INTEGER,ALLOCATABLE,DIMENSION(:) :: JBMPDATA
 INTEGER,DIMENSION(3) :: INFO  
 CHARACTER(LEN=256) :: LINE,FNAME
 REAL :: XMIN,YMIN,XMAX,YMAX,DX,DY,XE,XS,X1,X2,Y1,Y2
 
 IF(IBATCH.EQ.0)THEN
  CALL WINDOWOUTSTATUSBAR(2,''); CALL WINDOWOUTSTATUSBAR(3,''); CALL WINDOWOUTSTATUSBAR(4,'')
  CALL UTL_MESSAGEHANDLE(0)
  CALL WINDOWOUTSTATUSBAR(4,'Reading '//TRIM(BMPFNAME)//'...')
 ENDIF
 
 !## overrule setting read
 CALL IGRFILEINFO(BMPFNAME,INFO,3)
 NCOL=INFO(2); NROW=INFO(3)
 IF(INFO(1).EQ.-1)THEN
  IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error file:'//CHAR(13)// &
   TRIM(BMPFNAME)//CHAR(13)//'does not exist','Error')
  IF(IBATCH.EQ.1)WRITE(*,'(A)') 'Error file:'//TRIM(BMPFNAME)//' does not exist'
  RETURN
 ENDIF

 !## read obliged world file
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=TRIM(BMPFNAME)//'W',STATUS='OLD',ACTION='READ',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error file:'//CHAR(13)// &
   TRIM(BMPFNAME)//'W'//CHAR(13)//'does not exist','Error')
  IF(IBATCH.EQ.1)WRITE(*,'(A)') 'Error file:'//TRIM(BMPFNAME)//'W'//' does not exist'
 ENDIF
  
 READ(IU,*)  DX
 READ(IU,*)  
 READ(IU,*)  
 READ(IU,*)  DY
 READ(IU,*)  XMIN
 READ(IU,*)  YMAX
 CLOSE(IU)
 
 DY=ABS(DY); XMAX=XMIN+(NCOL*DX); YMIN=YMAX-(NROW*DY)

 IF(ALLOCATED(IBMPDATA))DEALLOCATE(IBMPDATA); ALLOCATE(IBMPDATA(NCOL*NROW),STAT=IOS)
 IF(IOS.NE.0)THEN
  IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not allocate enough memory to read file:'//CHAR(13)// &
   TRIM(BMPFNAME)//CHAR(13)//'NROW/NCOL='//TRIM(ITOS(NROW))//'/'//TRIM(ITOS(NCOL)),'Error')
  IF(IBATCH.EQ.1)WRITE(*,'(A,2I10)') 'Can not allocate enough memory to read file:'//TRIM(BMPFNAME)//' NROW/NCOL=',NROW,NCOL
  RETURN
 ENDIF
 IF(.NOT.UTL_LOADIMAGE(BMPFNAME,SIZE(IBMPDATA),IBMPDATA,IBATCH))THEN
  DEALLOCATE(IBMPDATA)
  RETURN 
 ENDIF
 
! I=WINFOERROR(1); CALL IGRLOADIMAGEDATA(BMPFNAME,IBMPDATA); I=WINFOERROR(1)
! IF(I.NE.0)THEN
!  DEALLOCATE(IBMPDATA)
!  CALL WINFOERRORMESSAGE(I,LINE)
!  IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error reading file:'//CHAR(13)// &
!   TRIM(BMPFNAME)//CHAR(13)//'Error message:'//CHAR(13)//TRIM(LINE),'Error')
!  IF(IBATCH.EQ.1)WRITE(*,'(A)') 'Error reading file:'//TRIM(BMPFNAME)//' Error message:'//TRIM(LINE)
!  RETURN
! ENDIF

 CALL UTL_CREATEDIR(OUTPUTFOLDER)
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=TRIM(OUTPUTFOLDER)//'\BMP.CRD',STATUS='UNKNOWN',ACTION='WRITE',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error file:'//CHAR(13)// &
   TRIM(OUTPUTFOLDER)//'\BMP.CRD'//CHAR(13)//' could not be created','Error')
  IF(IBATCH.EQ.1)WRITE(*,'(A)') 'Error file:'//TRIM(OUTPUTFOLDER)//'\BMP.CRD'//' could not be created'
 ENDIF
 
 IS=1
 XE=0.0
 DO IS=1,NS
  XS=XE; XE=XS+DXW
  LINE=TRIM(RTOS(XS,'F',2))//','//TRIM(RTOS(XE,'F',2))//',bmp'//TRIM(ITOS(IS))//'.TXT'
  WRITE(IU,'(A)') TRIM(LINE) 

  JU=UTL_GETUNIT()
  CALL OSD_OPEN(JU,FILE=TRIM(OUTPUTFOLDER)//'\bmp'//TRIM(ITOS(IS))//'.TXT',STATUS='UNKNOWN',ACTION='WRITE',IOSTAT=IOS)
   
  !## number of boxes
  NC=((NCOL/IS)/MXS)+1; NR=((NROW/IS)/MXS)+1
  DO IR=1,NR; DO IC=1,NC
   
   !## column/row original bitmap
   IC1=(IC-1)*MXS*IS+1; IC2=(IC)*MXS*IS
   IR1=(IR-1)*MXS*IS+1; IR2=(IR)*MXS*IS
   IC2=MIN(NCOL,IC2);   IR2=MIN(NROW,IR2)
       
   !## size of current bitmap to be created
   BNC=0; IF(MOD(IC2-IC1+1,IS).NE.0)BNC=1; BNC=BNC+(IC2-IC1+1)/IS
   BNR=0; IF(MOD(IR2-IR1+1,IS).NE.0)BNR=1; BNR=BNR+(IR2-IR1+1)/IS

   !## global coordinates
   X1=XMIN+REAL(IC1-1)*DX; X2=XMIN+REAL(IC2)*DX
   Y2=YMAX-REAL(IR1-1)*DY; Y1=YMAX-REAL(IR2)*DY

   IF(ALLOCATED(JBMPDATA))DEALLOCATE(JBMPDATA); ALLOCATE(JBMPDATA(BNC*BNR)) 
 
   DS=(IS-1)/2; KK=0; N=0
   DO I=IR1,IR2,IS; DO J=IC1,IC2,IS
    K=0; IRED=0; IGREEN=0; IBLUE=0
    DO II=MAX(1,I-DS),MIN(NROW,I+DS); DO JJ=MAX(1,J-DS),MIN(NCOL,J+DS)   
     CALL WRGBSPLIT(TOPO_GETCOLOR(NCOL,NROW,IBMPDATA,JJ,II),JRED,JGREEN,JBLUE)
     K=K+1; IRED=IRED+JRED; IGREEN=IGREEN+JGREEN; IBLUE=IBLUE+JBLUE 
    ENDDO; ENDDO
    JRED=IRED/K; JGREEN=IGREEN/K; JBLUE=IBLUE/K
    IF(JRED.NE.255.AND.JGREEN.NE.255.AND.JBLUE.NE.255)N=N+1
    KK=KK+1; JBMPDATA(KK)=WRGB(JRED,JGREEN,JBLUE)
   ENDDO; ENDDO
 
   IF(N.GT.0)THEN
    WRITE(FNAME,'(A,I2.2,A,2I2.2,A)') 'KB_',IS,'_',IR,IC,'.bmp'
    WRITE(JU,'(4F10.2,2I10,F10.2,A,2F10.2)') X1,Y1,X2,Y2,BNC,BNR,DX*REAL(IS),' '//TRIM(FNAME), X2-X1,Y2-Y1
    CALL IGRSAVEIMAGEDATA(TRIM(OUTPUTFOLDER)//'\'//TRIM(FNAME),JBMPDATA,BNC,BNR)
    WRITE(*,'(A)') 'Saving '//TRIM(OUTPUTFOLDER)//'\'//TRIM(FNAME)
   ENDIF
   DEALLOCATE(JBMPDATA)
  
  ENDDO; ENDDO
  CLOSE(JU)
  
 ENDDO
 
 CLOSE(IU)
 IF(IBATCH.EQ.0)CALL UTL_MESSAGEHANDLE(1)

 END SUBROUTINE TOPO1DRAW_TILING

 !###======================================================================
 INTEGER FUNCTION TOPO_GETCOLOR(NCOL,NROW,IBMPDATA,J,I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NCOL,NROW,J,I
 INTEGER,DIMENSION(NCOL,NROW) :: IBMPDATA
 
 TOPO_GETCOLOR=IBMPDATA(J,I)
 
 END FUNCTION TOPO_GETCOLOR

END MODULE MOD_TOPO
