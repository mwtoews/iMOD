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
MODULE MOD_MDF

USE WINTERACTER
USE RESOURCE
USE MODPLOT, ONLY : MXMPLOT,MP,MXCLR,MXCGRAD,MXCLASS,LEGENDOBJ
USE MOD_UTL, ONLY : UTL_GETUNIT,UTL_CAP,ITOS,RTOS,UTL_WSELECTFILE,UTL_DELSPACE
USE MOD_LEGEND_UTL, ONLY : LEG_WRITE_LEGEND,LEG_READ_LEGEND,LEG_ALLOCATE,LEG_DEALLOCATE
USE MOD_OSD, ONLY : OSD_OPEN,OSD_IOSTAT_MSG

TYPE MDFOBJ
 INTEGER :: PRFTYPE     ! prof.type line(0)/filled(1)
 INTEGER :: SCOLOR      ! color number for plotting
 INTEGER :: UNITS       ! units
 CHARACTER(LEN=256) :: FNAME
 CHARACTER(LEN=50) :: ALIAS
 INTEGER :: ISEL
 TYPE(LEGENDOBJ) :: LEG
END TYPE MDFOBJ
TYPE(MDFOBJ),DIMENSION(:),ALLOCATABLE :: MDF
INTEGER,ALLOCATABLE,DIMENSION(:),PRIVATE :: ILIST,JLIST

CONTAINS

 !###======================================================================
 SUBROUTINE MDF_MAIN(IPLOT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPLOT
 INTEGER :: ITYPE,I,N
 TYPE(WIN_MESSAGE) :: MESSAGE

 IF(.NOT.READMDF(MP(IPLOT)%IDFNAME,N))THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error reading MDF file:'//CHAR(13)//TRIM(MP(IPLOT)%IDFNAME),'Error')
  RETURN
 ENDIF

 CALL WDIALOGLOAD(ID_DMDFFILES,ID_DMDFFILES)

 CALL WDIALOGPUTIMAGE(ID_OPEN,ID_ICONOPENIDF,1)
 CALL WDIALOGPUTIMAGE(ID_DELETE,ID_ICONDELETE,1)
 CALL WDIALOGPUTIMAGE(ID_MOVEUP,ID_ICONMOVEUP,1)
 CALL WDIALOGPUTIMAGE(ID_MOVEDOWN,ID_ICONMOVEDOWN,1)
 CALL WDIALOGPUTIMAGE(ID_INFO,ID_ICONINFO,1)
 CALL WDIALOGPUTIMAGE(ID_LEGEND,ID_ICONLEGEND,1)

 ALLOCATE(ILIST(N+1))
 ILIST=0
 ILIST(MP(IPLOT)%NLIDF)=1
 CALL MDF_UPDATE(N)

 CALL MDF_FIELDS(N)
 CALL WDIALOGSHOW(-1,-1,0,3)

 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)
   CASE (FIELDCHANGED)
    SELECT CASE (MESSAGE%VALUE2)
     CASE (IDF_MENU1)
      CALL MDF_FIELDS(N)
     CASE (IDF_CHECK1)
      CALL WDIALOGGETMENU(IDF_MENU1,ILIST)
      CALL MDF_UPDATE(N)
    END SELECT
   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (ID_INFO)
      CALL WINDOWOPENCHILD(I,FLAGS=HIDEWINDOW,TITLE='File ')!//TRIM(FNAME))
      CALL WEDITFILE(MP(IPLOT)%IDFNAME,MODAL,0,0,COURIERNEW,ISIZE=8)
     CASE (ID_OPEN)
      CALL MDF_OPEN(N)
     CASE (ID_DELETE)
      CALL MDF_DELETE(N)

     CASE (ID_LEGEND)

     CASE (ID_MOVEUP,ID_MOVEDOWN)
      CALL MDF_MOVE(MESSAGE%VALUE1,N)
     CASE (IDOK,IDCANCEL)
      EXIT
     CASE (IDHELP)
      CALL IMODGETHELP('3.4.4','MMO.GroupIDF')     
    END SELECT
  END SELECT
 ENDDO

 IF(MESSAGE%VALUE1.EQ.IDOK)THEN
  CALL WDIALOGGETMENU(IDF_MENU1,ILIST)
  MP(IPLOT)%NLIDF=1
  DO I=1,N
   IF(ILIST(I).EQ.1)THEN
    MP(IPLOT)%NLIDF=I
    EXIT
   ENDIF
  ENDDO
  IF(.NOT.WRITEMDF(MP(IPLOT)%IDFNAME,N))THEN
  ENDIF
 ENDIF

 CALL WDIALOGUNLOAD()
 DEALLOCATE(ILIST)
 CALL MDFDEALLOCATE()

 IF(MESSAGE%VALUE1.EQ.IDOK)CALL IDFPLOT(1)

 END SUBROUTINE MDF_MAIN

 !###======================================================================
 SUBROUTINE MDF_OPEN(N)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(INOUT) :: N
 INTEGER :: I

 MDF(N+1)%FNAME=''
 IF(.NOT.UTL_WSELECTFILE('iMOD Map (*.idf)|*.idf|',&
                  LOADDIALOG+MUSTEXIST+PROMPTON+DIRCHANGE+APPENDEXT+MULTIFILE,MDF(N+1)%FNAME,&
                  'Load iMOD Map (*.idf)'))RETURN

 N=N+1
 IF(SIZE(ILIST).LT.N+1)THEN
  DEALLOCATE(ILIST)
  ALLOCATE(ILIST(N+1))
 ENDIF

 I=INDEX(MDF(N)%FNAME,'\',.TRUE.)+1
 MDF(N)%ALIAS =MDF(N)%FNAME(I:)
 IF(N.GT.1)THEN
  MDF(N)%LEG%NCLR  =MDF(N-1)%LEG%NCLR
  MDF(N)%LEG%CLASS =MDF(N-1)%LEG%CLASS
  MDF(N)%LEG%RGB   =MDF(N-1)%LEG%RGB
  MDF(N)%LEG%LEGTXT=MDF(N-1)%LEG%LEGTXT
  MDF(N)%LEG%CGRAD =MDF(N-1)%LEG%CGRAD
 ELSE
  MDF(N)%LEG%NCLR    = 1
  MDF(N)%LEG%CLASS(1)= 0.5
  MDF(N)%LEG%CLASS(0)=-0.5
  MDF(N)%LEG%RGB     = WRGB(255,0,0)
  MDF(N)%LEG%CGRAD   = 1
  MDF(N)%LEG%LEGTXT  = 'dummy'
 ENDIF

 ILIST=0
 ILIST(N)=1
 CALL MDF_UPDATE(N)

 END SUBROUTINE MDF_OPEN

 !###======================================================================
 SUBROUTINE MDF_UPDATE(N)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 INTEGER :: I

 CALL WDIALOGGETCHECKBOX(IDF_CHECK1,I)
 IF(I.EQ.0)CALL WDIALOGPUTMENU(IDF_MENU1,MDF%ALIAS,N,ILIST)
 IF(I.EQ.1)CALL WDIALOGPUTMENU(IDF_MENU1,MDF%FNAME,N,ILIST)

 END SUBROUTINE MDF_UPDATE

 !###======================================================================
 SUBROUTINE MDF_DELETE(N)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(INOUT) :: N
 INTEGER :: I,J

 CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Are you sure to delete the selected files from the MDF-file'//CHAR(13)// &
  'Files will be NOT deleted from the disk!','Question')
 IF(WINFODIALOG(4).NE.1)RETURN

 CALL WDIALOGGETMENU(IDF_MENU1,ILIST)
 I=0
 DO
  I=I+1
  IF(I.GT.N)EXIT
  IF(ILIST(I).EQ.1)THEN
   DO J=I,N-1
    MDF(J)  =MDF(J+1)
    ILIST(J)=ILIST(J+1)
   END DO
   N=N-1
   I   =I-1
  ENDIF
 END DO

 CALL MDF_UPDATE(N)

 END SUBROUTINE MDF_DELETE

 !###======================================================================
 SUBROUTINE MDF_FIELDS(N)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 INTEGER :: I

 CALL WDIALOGGETMENU(IDF_MENU1,ILIST)
 I=MIN(1,SUM(ILIST(1:N)))
 IF(ILIST(1).EQ.1)I=0
 CALL WDIALOGFIELDSTATE(ID_MOVEUP,I)
 I=MIN(1,SUM(ILIST(1:N)))
 IF(ILIST(N).EQ.1)I=0
 CALL WDIALOGFIELDSTATE(ID_MOVEDOWN,I)

 I=MIN(1,SUM(ILIST(1:N)))
 CALL WDIALOGFIELDSTATE(ID_DELETE,I)

 END SUBROUTINE MDF_FIELDS

 !###======================================================================
 SUBROUTINE MDF_MOVE(ID,N)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID,N
 INTEGER :: I

 CALL WDIALOGGETMENU(IDF_MENU1,ILIST)
 DO I=1,N
  MDF(I)%ISEL=ILIST(I)
 END DO

 IF(ID.EQ.ID_MOVEUP)THEN
  DO I=1,N
   IF(ILIST(I).EQ.1)THEN
    MDF(N+1)=MDF(I-1)
    MDF(I-1)=MDF(I)
    MDF(I)  =MDF(N+1)
   ENDIF
  ENDDO
 ELSEIF(ID.EQ.ID_MOVEDOWN)THEN
  DO I=N-1,1,-1
   IF(ILIST(I).EQ.1)THEN
    MDF(N+1)=MDF(I+1)
    MDF(I+1)=MDF(I)
    MDF(I)  =MDF(N+1)
   ENDIF
  ENDDO
 ENDIF

 DO I=1,N
  ILIST(I)=MDF(I)%ISEL
 END DO

 !##  fill manager
 CALL MDF_UPDATE(N)
 CALL MDF_FIELDS(N)

 END SUBROUTINE MDF_MOVE

 !###======================================================================
 INTEGER FUNCTION READMDF_GETN(FNAME)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER :: IOS,IU

 IU=UTL_GETUNITMDF(FNAME,'OLD')
 !## error opening file
 IF(IU.LE.0)RETURN

 READ(IU,*,IOSTAT=IOS) READMDF_GETN
 IF(IOS.NE.0)READMDF_GETN=0

 CLOSE(IU)

 END FUNCTION READMDF_GETN

 !###======================================================================
 LOGICAL FUNCTION READMDF(FNAME,N)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: N
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER :: IOS,I,IU
 CHARACTER(LEN=256) :: LINE

 READMDF=.FALSE.

 IU=UTL_GETUNITMDF(FNAME,'OLD')
 !## error opening file
 IF(IU.LE.0)RETURN

 READ(IU,'(A256)',IOSTAT=IOS) LINE
 IF(IOS.NE.0)RETURN
 READ(LINE,*,IOSTAT=IOS) N
 IF(IOS.NE.0.OR.N.LE.0)THEN
  CLOSE(IU)
  RETURN
 ENDIF

 CALL MDFDEALLOCATE()
 CALL MDFALLOCATE()

 CALL WINDOWSELECT(0); CALL WINDOWOUTSTATUSBAR(4,'Reading '//TRIM(FNAME)//'...')

 !## read selected idf, use that for plotting purposes
 DO I=1,N
  READ(IU,'(A256)',IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT
  READ(LINE,*,IOSTAT=IOS) MDF(I)%FNAME,MDF(I)%ALIAS,MDF(I)%SCOLOR,MDF(I)%PRFTYPE,MDF(I)%UNITS
  IF(IOS.NE.0)THEN
   MDF(I)%UNITS=0
   READ(LINE,*,IOSTAT=IOS) MDF(I)%FNAME,MDF(I)%ALIAS,MDF(I)%SCOLOR,MDF(I)%PRFTYPE
   IF(IOS.NE.0)EXIT
  ENDIF
  !## read legend
  CALL LEG_READ_LEGEND(IU,MDF(I)%LEG)
 ENDDO
 IF(I.GT.N)READMDF=.TRUE.
 IF(.NOT.READMDF)CALL MDFDEALLOCATE()
 
 CALL WINDOWSELECT(0); CALL WINDOWOUTSTATUSBAR(4,'')
 
 CLOSE(IU)

 END FUNCTION READMDF

 !###======================================================================
 LOGICAL FUNCTION WRITEMDF(FNAME,N)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER :: IOS,I,IU,N
 CHARACTER(LEN=256) :: LINE,L1

 WRITEMDF=.FALSE.

 IU=UTL_GETUNITMDF(FNAME,'UNKNOWN')
 !## error opening file
 IF(IU.LE.0)RETURN

 LINE=TRIM(ITOS(N))
 WRITE(IU,*,IOSTAT=IOS) TRIM(LINE)

 CALL WINDOWSELECT(0); CALL WINDOWOUTSTATUSBAR(4,'Saving '//TRIM(FNAME)//'...')
 
 !## read selected idf, use that for plotting purposes
 DO I=1,N
  WRITE(L1,*) '"'//TRIM(MDF(I)%FNAME)//'","'//TRIM(MDF(I)%ALIAS)//'",',MDF(I)%SCOLOR,',',MDF(I)%PRFTYPE,',',MDF(I)%UNITS
  CALL UTL_DELSPACE(L1,LINE) 
  WRITE(IU,'(A)',IOSTAT=IOS) TRIM(LINE)
  IF(IOS.NE.0)EXIT
  !## write legend
  CALL LEG_WRITE_LEGEND(IU,MDF(I)%LEG)
 ENDDO
 IF(I.GT.N)WRITEMDF=.TRUE.

 CLOSE(IU)
 CALL WINDOWSELECT(0); CALL WINDOWOUTSTATUSBAR(4,'')
 
 END FUNCTION WRITEMDF

 !###======================================================================
 INTEGER FUNCTION UTL_GETUNITMDF(MDFNAME,TSTAT)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: MDFNAME,TSTAT
 LOGICAL :: LEX,LOPEN
 CHARACTER(LEN=10) :: TSTATUS
 CHARACTER(LEN=50) :: MESSAGE
 INTEGER :: IOS

 TSTATUS   =UTL_CAP(TSTAT,'U')
 UTL_GETUNITMDF=0

 INQUIRE(FILE=MDFNAME,OPENED=LOPEN)
 IF(LOPEN)THEN
  INQUIRE(FILE=MDFNAME,NUMBER=UTL_GETUNITMDF)
  CLOSE(UTL_GETUNITMDF)
  UTL_GETUNITMDF=0
 ENDIF
 IF(TSTATUS(1:3).EQ.'OLD')THEN
  INQUIRE(FILE=MDFNAME,EXIST=LEX)
  IF(.NOT.LEX)THEN
   IF(TSTATUS(4:4).NE.'1')CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not find'//CHAR(13)//TRIM(MDFNAME),'Warning')
   RETURN
  ENDIF
  TSTATUS=TSTAT(1:3)
  UTL_GETUNITMDF=UTL_GETUNIT()
  CALL OSD_OPEN(UTL_GETUNITMDF,FILE=MDFNAME,STATUS=TSTATUS,FORM='FORMATTED',ACTION='READ,DENYWRITE',IOSTAT=IOS)
  IF(IOS.NE.0)THEN
   UTL_GETUNITMDF=0
   CALL OSD_IOSTAT_MSG(IOS,MESSAGE)
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not open'//CHAR(13)//TRIM(MDFNAME)//CHAR(13)// &
   TRIM(MESSAGE),'Warning')
  ENDIF
 ELSEIF(TSTATUS(1:7).EQ.'UNKNOWN')THEN
  UTL_GETUNITMDF=UTL_GETUNIT()
  CALL OSD_OPEN(UTL_GETUNITMDF,FILE=MDFNAME,STATUS=TSTATUS,FORM='FORMATTED',ACTION='WRITE,DENYREAD',IOSTAT=IOS)
  IF(IOS.NE.0)THEN
   UTL_GETUNITMDF=0
   CALL OSD_IOSTAT_MSG(IOS,MESSAGE)
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not create'//CHAR(13)//TRIM(MDFNAME)//CHAR(13)// &
   TRIM(MESSAGE),'Warning')
  ENDIF
 ENDIF

 END FUNCTION UTL_GETUNITMDF

 !###======================================================================
 SUBROUTINE MDFALLOCATE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 ALLOCATE(MDF(MXMPLOT))
 DO I=1,MXMPLOT
!  NULLIFY(MDF(I)%LEG%CLASS)
!  NULLIFY(MDF(I)%LEG%LEGTXT)
!  NULLIFY(MDF(I)%LEG%RGB)
!  NULLIFY(MDF(I)%LEG%CGRAD)
  MDF(I)%LEG%NCLR=0
  CALL LEG_ALLOCATE(MDF(I)%LEG)
 END DO

 END SUBROUTINE MDFALLOCATE

 !###======================================================================
 SUBROUTINE MDFDEALLOCATE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 IF(.NOT.ALLOCATED(MDF))RETURN
 DO I=1,SIZE(MDF)
!  IF(ASSOCIATED(MDF(I)%LEG%LEGTXT))DEALLOCATE(MDF(I)%LEG%LEGTXT)
!  IF(ASSOCIATED(MDF(I)%LEG%CGRAD))DEALLOCATE(MDF(I)%LEG%CGRAD)
!  IF(ASSOCIATED(MDF(I)%LEG%RGB))DEALLOCATE(MDF(I)%LEG%RGB)
!  IF(ASSOCIATED(MDF(I)%LEG%CLASS))DEALLOCATE(MDF(I)%LEG%CLASS)
  CALL LEG_DEALLOCATE(MDF(I)%LEG)
 END DO
 DEALLOCATE(MDF)

 END SUBROUTINE MDFDEALLOCATE

END MODULE MOD_MDF

