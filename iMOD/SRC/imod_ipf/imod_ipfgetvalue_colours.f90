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
MODULE MOD_IPFGETVALUE_COLOURS

USE WINTERACTER
USE RESOURCE
USE MOD_UTL, ONLY : ITOS,RTOS,UTL_GETUNIT,UTL_WSELECTFILE,UTL_DEBUGLEVEL
USE MOD_IPF_PAR, ONLY : ASSF,BH,NLITHO,MAXLITHO
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_OSD, ONLY : OSD_OPEN

CONTAINS

 !###======================================================================
 SUBROUTINE IPFGETVALUE_PLOTCOLOURS(DID,ILEG)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: DID,ILEG
 INTEGER :: I,MAXNROW

 CALL WDIALOGSELECT(DID)
 MAXNROW=WINFOGRID(IDF_GRID1,GRIDROWSMAX)
 CALL WDIALOGCLEARFIELD(IDF_GRID1)
 
 DO I=1,MIN(MAXNROW,NLITHO(ILEG))
  CALL WGRIDLABELROW(      IDF_GRID1,I,TRIM(ITOS(I)))
  CALL WGRIDPUTCELLSTRING( IDF_GRID1,1,I,BH(ILEG,I)%LITHO)
  CALL WGRIDCOLOURCELL(    IDF_GRID1,2,I,BH(ILEG,I)%LITHOCLR,BH(ILEG,I)%LITHOCLR)
  CALL WGRIDPUTCELLINTEGER(IDF_GRID1,2,I,BH(ILEG,I)%LITHOCLR)
  CALL WGRIDPUTCELLSTRING( IDF_GRID1,3,I,TRIM(ADJUSTL(BH(ILEG,I)%LITHOTXT)))
  CALL WGRIDPUTCELLREAL(   IDF_GRID1,4,I,BH(ILEG,I)%LITHOWIDTH,'(F7.2)')
 ENDDO

 DO I=NLITHO(ILEG)+1,MAXNROW
  CALL WGRIDLABELROW(      IDF_GRID1,I,TRIM(ITOS(I)))
  CALL WGRIDPUTCELLSTRING( IDF_GRID1,1,I,BH(ILEG,I)%LITHO)
  CALL WGRIDCOLOURCELL(    IDF_GRID1,2,I,BH(ILEG,I)%LITHOCLR,BH(ILEG,I)%LITHOCLR)
  CALL WGRIDPUTCELLINTEGER(IDF_GRID1,2,I,BH(ILEG,I)%LITHOCLR)
  CALL WGRIDCLEARCELL(     IDF_GRID1,3,I)
  CALL WGRIDCLEARCELL(     IDF_GRID1,4,I)
 ENDDO

 END SUBROUTINE IPFGETVALUE_PLOTCOLOURS

 !###======================================================================
 SUBROUTINE IPFGETVALUE_GETCOLOURS(DID,ILEG)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: DID
 INTEGER,INTENT(INOUT) :: ILEG
 INTEGER :: MAXNROW,I,J

 CALL WDIALOGSELECT(DID)
 MAXNROW=WINFOGRID(IDF_GRID1,GRIDROWSMAX)
 CALL WDIALOGUNDEFINED(-999)

 ILEG=MIN(MAX(1,ILEG),SIZE(BH,1)) 
 
 !# initial values
 DO I=1,MAXLITHO
  BH(ILEG,I)%LITHO=''
  BH(ILEG,I)%LITHOCLR=WRGB(255,255,255)
  BH(ILEG,I)%LITHOWIDTH=1.0
  BH(ILEG,I)%LITHOTXT=''
 ENDDO
 
 J=0
 CALL UTL_DEBUGLEVEL(0)
 DO I=1,MAXNROW 
  CALL WGRIDGETCELLSTRING(IDF_GRID1,1,I,BH(ILEG,I)%LITHO)
  CALL WGRIDGETCELLINTEGER(IDF_GRID1,2,I,BH(ILEG,I)%LITHOCLR)
  CALL WGRIDGETCELLREAL(IDF_GRID1,4,I,BH(ILEG,I)%LITHOWIDTH)
  IF(BH(ILEG,I)%LITHO.NE.''.AND.BH(ILEG,I)%LITHOCLR.GE.0)THEN
   J=J+1
   BH(ILEG,J)%LITHO     =BH(ILEG,I)%LITHO
   BH(ILEG,J)%LITHOCLR  =BH(ILEG,I)%LITHOCLR
   BH(ILEG,J)%LITHOWIDTH=MAX(0.0,BH(ILEG,I)%LITHOWIDTH)
   CALL WGRIDGETCELLSTRING(IDF_GRID1,3,I,BH(ILEG,J)%LITHOTXT)
  ENDIF
 ENDDO
 CALL UTL_DEBUGLEVEL(1)

 NLITHO(ILEG)=J 

 END SUBROUTINE IPFGETVALUE_GETCOLOURS

 !###======================================================================
 SUBROUTINE IPFGETVALUE_OPENSAVECOLOURS(DLFNAME,ID,DID,ILEG)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DLFNAME
 INTEGER,INTENT(IN) :: ID,DID
 INTEGER,INTENT(INOUT) :: ILEG
 CHARACTER(LEN=256) :: FNAME,LINE
 INTEGER :: IU,IOS,I,IR,IG,IB
 LOGICAL :: LEX

 IF(LEN_TRIM(DLFNAME).EQ.0)THEN
  FNAME=TRIM(PREFVAL(1))//'\SETTINGS\*.dlf'
  IF(ID.EQ.ID_SAVEAS)THEN
   IF(.NOT.UTL_WSELECTFILE('iMOD Borehole Legend File (*.dlf)|*.dlf|', &
         SAVEDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,FNAME,'Save iMOD Borehole Legend file'))RETURN
  ELSEIF(ID.EQ.ID_OPEN)THEN
   IF(.NOT.UTL_WSELECTFILE('iMOD Borehole File (*.dlf)|*.dlf|',&
         LOADDIALOG+MUSTEXIST+DIRCHANGE+APPENDEXT,FNAME,'Load iMOD Borehole Legend file'))RETURN
  ENDIF
 ELSE
  !## try to read given dlf name
  FNAME=DLFNAME; INQUIRE(FILE=FNAME,EXIST=LEX); IF(.NOT.LEX)RETURN
 ENDIF

 IU=UTL_GETUNIT()
 IF(ID.EQ.ID_SAVEAS)CALL OSD_OPEN(IU,FILE=TRIM(FNAME),STATUS='REPLACE',FORM='FORMATTED',IOSTAT=IOS)
 IF(ID.EQ.ID_OPEN)CALL OSD_OPEN(IU,FILE=TRIM(FNAME),STATUS='OLD',FORM='FORMATTED',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not open iMOD Borehole Legend file, no permission!','ERROR')
  RETURN
 ENDIF

 !## save definitions
 IF(ID.EQ.ID_SAVEAS)THEN
  !## (re)read legend
  CALL IPFGETVALUE_GETCOLOURS(DID,ILEG)
  WRITE(IU,'(A)') 'Label,Ired,Igreen,Iblue,Label-text'
  DO I=1,NLITHO(ILEG)
   CALL WRGBSPLIT(BH(ILEG,I)%LITHOCLR,IR,IG,IB)
   LINE='"'//TRIM(BH(ILEG,I)%LITHO)//'",'//TRIM(ITOS(IR))//','//TRIM(ITOS(IG))//','// &
             TRIM(ITOS(IB))//',"'//TRIM(BH(ILEG,I)%LITHOTXT)//'",'//TRIM(RTOS(BH(ILEG,I)%LITHOWIDTH,'F',2))
   WRITE(IU,'(A)') TRIM(LINE)
  ENDDO

 !## read new definitions
 ELSEIF(ID.EQ.ID_OPEN)THEN
  READ(IU,*,IOSTAT=IOS)
  NLITHO(ILEG)=0
  DO
   READ(IU,'(A256)',IOSTAT=IOS) LINE
   IF(IOS.NE.0)EXIT; NLITHO(ILEG)=NLITHO(ILEG)+1
  ENDDO
  IF(NLITHO(ILEG).GT.0)THEN

   DO I=1,MAXLITHO
    BH(ILEG,I)%LITHO     =''
    BH(ILEG,I)%LITHOTXT  =''
    BH(ILEG,I)%LITHOWIDTH=1.0
    BH(ILEG,I)%LITHOCLR  =WRGB(255,255,255)
   ENDDO
   
   REWIND(IU)
   READ(IU,*,IOSTAT=IOS)

   I=0
   DO
    READ(IU,'(A256)',IOSTAT=IOS) LINE
    IF(IOS.NE.0)EXIT
    IF(I+1.GT.SIZE(BH))THEN
     CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Maximum of '//TRIM(ITOS(SIZE(BH)))//' records read in:'//CHAR(13)// &
      TRIM(FNAME),'Warning')
     EXIT
    ENDIF
    I=I+1
    READ(LINE,*,IOSTAT=IOS) BH(ILEG,I)%LITHO,IR,IG,IB,BH(ILEG,I)%LITHOTXT,BH(ILEG,I)%LITHOWIDTH
    IF(IOS.NE.0)THEN
     BH(ILEG,I)%LITHOWIDTH=1.0
     READ(LINE,*,IOSTAT=IOS) BH(ILEG,I)%LITHO,IR,IG,IB,BH(ILEG,I)%LITHOTXT
     IF(IOS.NE.0)THEN
      CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error reading '//TRIM(FNAME)//CHAR(13)//'line '//TRIM(ITOS(I+1)),'Error')
      I=I-1
      EXIT
     ENDIF
    ENDIF
    BH(ILEG,I)%LITHOWIDTH=MAX(0.0,BH(ILEG,I)%LITHOWIDTH)
    BH(ILEG,I)%LITHOCLR=WRGB(IR,IG,IB)
   ENDDO
   NLITHO(ILEG)=I

   IF(DID.NE.0)CALL IPFGETVALUE_PLOTCOLOURS(DID,ILEG)

  ELSE
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Nothing in '//TRIM(FNAME),'Error')
  ENDIF

 ENDIF

 CLOSE(IU)

 END SUBROUTINE IPFGETVALUE_OPENSAVECOLOURS

END MODULE MOD_IPFGETVALUE_COLOURS

