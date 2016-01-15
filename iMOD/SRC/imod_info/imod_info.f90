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
MODULE MOD_INFO

USE WINTERACTER
USE RESOURCE
USE MOD_UTL, ONLY : ITOS,RTOS,UTL_GETUNIT,UTL_STDEF,UTL_GETMED,NEWLINE
USE MODPLOT
USE MOD_IDF, ONLY : IDFREAD,IDFREADPART,IDFDEALLOCATEX,IDFGETCOMMENT,IDFNULLIFY, &
  IDFOPEN,IDFREADDIM,IDFREADDATA,IDFWRITE,IDFDEALLOCATE,IDFCOPY,IDFIROWICOL, &
  IDFALLOCATEX,IDFWRITECOMMENT,IDFFILLCOMMENT2
USE MOD_IDF_PAR, ONLY : NIDFTRANSFORM,IDFTRANSFORM,IDFOBJ
USE MOD_MDF, ONLY : READMDF,MDF_MAIN,MDFDEALLOCATE,MDF
USE MOD_OSD, ONLY : OSD_OPEN,OSD_IOSTAT_MSG,OSD_GETENV,OSD_DATE_AND_TIME,ICF
USE MOD_GENPLOT, ONLY : GENDATAGRID
USE MOD_PROFILE_UTL, ONLY : PROFILE_PLOTGRAPH,GRAPH,PROFILE_DEALLGRAPH,PROFILE_ALLGRAPH
USE MOD_IDFEDIT_TABLE, ONLY : UTL_EDITTABLE_INIT
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_IDFEDIT, ONLY : IDFEDITNODATA

TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:),PRIVATE :: IDF
CHARACTER(LEN=2560),PRIVATE :: STRING

CONTAINS

 !###======================================================================
 SUBROUTINE INFOMAIN()
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITYPE,J,IPLOT
 LOGICAL :: LEX,LADJ,LTB,LXY
 REAL :: IDF_OLDND

 CALL WDIALOGSELECT(ID_DMANAGERTAB1)
 CALL WDIALOGGETMENU(ID_DMTABMENU,ACTLIST)

 CALL WDIALOGLOAD(ID_DINFO,ID_DINFO)

 !#select first selected file
 DO IPLOT=1,MXMPLOT; IF(MP(IPLOT)%ISEL)EXIT; ENDDO
 IF(IPLOT.GT.MXMPLOT)IPLOT=1

 CALL WDIALOGPUTMENU(IDF_MENU1,MP%ALIAS,MPW%NACT,IPLOT)
 CALL WDIALOGPUTMENU(IDF_MENU2,IDFTRANSFORM,NIDFTRANSFORM,1)

 CALL WDIALOGPUTREAL(ID_XMIN2,MPW%XMIN,'(G15.7)')
 CALL WDIALOGPUTREAL(ID_XMAX2,MPW%XMAX,'(G15.7)')
 CALL WDIALOGPUTREAL(ID_DX2,(MPW%XMAX-MPW%XMIN),'(G15.7)')
 CALL WDIALOGPUTREAL(ID_YMIN2,MPW%YMIN,'(G15.7)')
 CALL WDIALOGPUTREAL(ID_YMAX2,MPW%YMAX,'(G15.7)')
 CALL WDIALOGPUTREAL(ID_DY2,(MPW%YMAX-MPW%YMIN),'(G15.7)')
 CALL WDIALOGPUTIMAGE(ID_INFO,ID_ICONINFO,1)
 CALL WDIALOGPUTIMAGE(ID_STAT,ID_ICONSOMVAL,1)
 CALL WDIALOGPUTIMAGE(ID_RENAME,ID_ICONRENAME,1)
 CALL WDIALOGPUTIMAGE(ID_EDIT,ID_ICONEDIT,1)

 CALL WDIALOGFIELDSTATE(ID_ADJUST,1)
 CALL WDIALOGFIELDSTATE(ID_ADJUSTTB,3)
 CALL WDIALOGFIELDSTATE(IDF_NOSAVE,3)
 
 CALL INFOFILL()

 CALL WDIALOGSHOW(-1,-1,1,3)
 LADJ=.FALSE.; LEX =.FALSE.; LTB =.FALSE.; LXY =.FALSE.
 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
   SELECT CASE (ITYPE)

    CASE (FIELDCHANGED)
     SELECT CASE (MESSAGE%VALUE1)
      CASE (IDF_MENU1)
       CALL INFOFILL()
     END SELECT

    CASE (PUSHBUTTON)
     SELECT CASE (MESSAGE%VALUE1)

     CASE (ID_EDIT)
      CALL INFOEDIT()
      CALL INFOFILL()
      
     CASE (ID_RENAME)
      CALL WDIALOGGETSTRING(IDF_STRING4,MP(IPLOT)%ALIAS)

     CASE (IDHELP)
      CALL IMODGETHELP('3.4.3','MMO.MapInfo')

     CASE (IDCANCEL)
      EXIT

     CASE (ID_STAT)
      CALL WDIALOGGETMENU(IDF_MENU1,IPLOT)
      CALL INFOSTAT(MP(IPLOT)%IDFNAME,0,0)
      
     CASE (ID_INFO)
      CALL WDIALOGGETMENU(IDF_MENU1,IPLOT)
      IF(MP(IPLOT)%IPLOT.EQ.1)CALL UTL_EDITTABLE_INIT(IPLOT)
      IF(MP(IPLOT)%IPLOT.EQ.5)CALL MDF_MAIN(IPLOT)
      IF(MP(IPLOT)%IPLOT.EQ.6)CALL GENDATAGRID(MP(IPLOT)%IDFNAME(:INDEX(MP(IPLOT)%IDFNAME,'.',.TRUE.)-1)//'.dat')
      CALL WDIALOGSELECT(ID_DINFO)
      CALL INFOFILL()

     CASE (ID_ADJUST)
      IF(.NOT.LEX)THEN
       CALL WDIALOGPUTSTRING(ID_ADJUST,'Apply')
       CALL WDIALOGFIELDSTATE(IDF_NOSAVE,1)
       CALL WDIALOGGETREAL(ID_NODATA,IDF_OLDND) !## store current nodata value to use in adjusting IDF-file after changing ND-value
       J=1; LEX=.TRUE.
      ELSE
       CALL WDIALOGPUTSTRING(ID_ADJUST,'Adjust')
       CALL WDIALOGFIELDSTATE(IDF_NOSAVE,3)
       IF(INFOADJUST(1))THEN; J=2; LEX=.FALSE.; LADJ=.TRUE.; ENDIF
      ENDIF
      CALL WDIALOGFIELDSTATE(ID_NODATA,J)
      CALL WDIALOGFIELDSTATE(IDF_MENU2,J)
      IF(J.EQ.2)THEN 
       CALL IDFEDITNODATA(IDF_OLDND,IPLOT)!## updates IDF-file with new NODATA values
       CALL INFOFILL()
      ENDIF
      
     CASE (ID_ADJUSTTB)
      IF(.NOT.LTB)THEN
       CALL WDIALOGPUTSTRING(ID_ADJUSTTB,'Store')
       J=1; LTB=.TRUE.
      ELSE
       CALL WDIALOGPUTSTRING(ID_ADJUSTTB,'Adjust')
       IF(INFOADJUST(2))THEN; J=2; LTB=.FALSE.; LADJ=.TRUE.; ENDIF
      ENDIF
      CALL WDIALOGFIELDSTATE(IDF_REAL1,J);   CALL WDIALOGFIELDSTATE(IDF_REAL2,J)
      CALL WDIALOGFIELDSTATE(IDF_LABEL25,J); CALL WDIALOGFIELDSTATE(IDF_LABEL26,J)
      CALL WDIALOGFIELDSTATE(IDF_LABEL27,J); CALL WDIALOGFIELDSTATE(IDF_LABEL28,J)
      IF(J.EQ.2)CALL INFOFILL()      

     CASE (ID_ADJUSTXY)
      IF(.NOT.LXY)THEN
       CALL WDIALOGPUTSTRING(ID_ADJUSTXY,'Save Adjustment')
       J=1; LXY=.TRUE.
      ELSE
       CALL WDIALOGPUTSTRING(ID_ADJUSTXY,'Adjust Lower Left Corner')
       IF(INFOADJUST(1))THEN; J=2; LXY=.FALSE.; LADJ=.TRUE.; ENDIF
      ENDIF
      CALL WDIALOGFIELDSTATE(ID_XMIN,J); CALL WDIALOGFIELDSTATE(ID_YMIN,J)
      IF(J.EQ.2)CALL INFOFILL()      

     CASE (IDF_NOSAVE)
      CALL WDIALOGPUTSTRING(ID_ADJUST,'Adjust')
      CALL WDIALOGFIELDSTATE(IDF_NOSAVE,3)
      !##restore nodata value
      CALL WDIALOGPUTREAL(ID_NODATA,MP(IPLOT)%IDF%NODATA,'(G15.7)')
      CALL WDIALOGPUTOPTION(IDF_MENU2,MP(IPLOT)%UNITS+1)
      CALL WDIALOGFIELDSTATE(ID_NODATA,2)
      CALL WDIALOGFIELDSTATE(IDF_MENU2,2)
      LEX=.FALSE.

     CASE (ID_MORE)
      CALL INFOMETA()

    END SELECT

   END SELECT
 ENDDO

 CALL WDIALOGSELECT(ID_DINFO)
 CALL WDIALOGUNLOAD()
 IF(LADJ)CALL IDFPLOTFAST(0)

 END SUBROUTINE INFOMAIN

 !###======================================================================
 SUBROUTINE INFOEDIT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IU,I,IWIN,IPLOT,IOS
 CHARACTER(LEN=256) :: FNAME,LINE
 CHARACTER(LEN=52) :: DATESTRING
 
 CALL WDIALOGGETMENU(IDF_MENU1,IPLOT)
 CALL WDIALOGGETSTRING(IDF_STRING2,MP(IPLOT)%IDF%FNAME)

 MP(IPLOT)%IDF%IU=UTL_GETUNIT()
 IF(ICF.EQ.0)THEN
  CALL OSD_OPEN(MP(IPLOT)%IDF%IU,FILE=MP(IPLOT)%IDF%FNAME,STATUS='OLD',FORM='UNFORMATTED',ACCESS='DIRECT', &
                RECL=4,ACTION='READWRITE',IOSTAT=IOS)
 ELSEIF(ICF.EQ.1)THEN
  CALL OSD_OPEN(MP(IPLOT)%IDF%IU,FILE=MP(IPLOT)%IDF%FNAME,STATUS='OLD',FORM='UNFORMATTED',ACCESS='DIRECT', &
                RECL=1,ACTION='READWRITE',IOSTAT=IOS)
 ENDIF
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not adjust current file'//CHAR(13)// &
    TRIM(MP(IPLOT)%IDF%FNAME)//CHAR(13)//'IDF has been marked probably as READ-ONLY','Error')
  IF(MP(IPLOT)%IDF%IU.GT.0)CLOSE(IU) !,STATUS='DELETE')
  RETURN
 ENDIF

 !## read current comment
 CALL IDFGETCOMMENT(MP(IPLOT)%IDF,0)

 FNAME=TRIM(PREFVAL(1))//'\comments_'//TRIM(OSD_GETENV('USERNAME'))//'.txt'
 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=FNAME,STATUS='UNKNOWN',ACTION='WRITE')
 
 IF(ASSOCIATED(MP(IPLOT)%IDF%COMMENT))THEN
  STRING=''
  DO I=1,SIZE(MP(IPLOT)%IDF%COMMENT)
   IF(I.EQ.1)THEN
    STRING=MP(IPLOT)%IDF%COMMENT(I)
   ELSE
    STRING=STRING(1:(I-1)*4)//MP(IPLOT)%IDF%COMMENT(I)
   ENDIF
  ENDDO
  WRITE(IU,'(A)') TRIM(STRING)
 ELSE
  WRITE(IU,*) '# General Information'
  WRITE(IU,*) '- Filename : '//TRIM(MP(IPLOT)%IDFNAME)
  CALL OSD_DATE_AND_TIME(DATEANDTIME=DATESTRING) 
  WRITE(IU,*) '- Publication Date : '//TRIM(DATESTRING)
  WRITE(IU,*) '- Version Number : '
  WRITE(IU,*) '- Comment : '
  WRITE(IU,*) '# Description Data'
  WRITE(IU,*) '- Unit : '
  WRITE(IU,*) '- Resolution : '
  WRITE(IU,*) '- Source : '
  WRITE(IU,*) '# Administration'
  WRITE(IU,*) '- Organisation : Deltares'
  WRITE(IU,*) '- Website : www.deltares.nl'
  WRITE(IU,*) '- Contactperson : '//TRIM(OSD_GETENV('USERNAME'))
 ENDIF 
 CLOSE(IU)
       
 CALL WINDOWOPENCHILD(IWIN,FLAGS=SYSMENUON+OWNEDBYPARENT+NOFILESAVEAS,WIDTH=500,HEIGHT=400)
 CALL WEDITFILE(FNAME,ITYPE=MODAL,IDMENU=0, &
                IFLAGS=MUSTEXIST+WORDWRAP+NOFILENEWOPEN+NOFILESAVEAS+NOFILECLOSE,IFONT=TIMESNEWROMAN)

 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=FNAME,STATUS='UNKNOWN',ACTION='READ')
 IF(IU.GT.0)THEN
  STRING=''
  I=0; DO
   READ(IU,'(A256)',IOSTAT=IOS) LINE
   IF(IOS.NE.0)EXIT
   IF(I.EQ.0)THEN
    STRING=TRIM(LINE)
   ELSE
    STRING=TRIM(STRING)//NEWLINE//TRIM(LINE)
   ENDIF
   I=I+1
  ENDDO
  CLOSE(IU) !,STATUS='DELETE')
 
  CALL IDFFILLCOMMENT2(MP(IPLOT)%IDF,TRIM(STRING))
  CALL IDFWRITECOMMENT(MP(IPLOT)%IDF,0)
 ENDIF
 
 CLOSE(MP(IPLOT)%IDF%IU); MP(IPLOT)%IDF%IU=0
 
 CALL IDFDEALLOCATEX(MP(IPLOT)%IDF)
 
 END SUBROUTINE INFOEDIT
 
 !###======================================================================
 SUBROUTINE INFOSTAT(FNAME,IU,I,XMIN,YMIN,XMAX,YMAX)
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER,INTENT(IN) :: IU,I
 INTEGER :: ITYPE,NPOP,J
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 REAL,INTENT(IN),OPTIONAL :: XMIN,YMIN,XMAX,YMAX
 REAL :: VAR,MEAN
 CHARACTER(LEN=1000) :: LINE
 
 ALLOCATE(IDF(1))
 DO J=1,SIZE(IDF); CALL IDFNULLIFY(IDF(J)); ENDDO
 !## fill idf%v instead of idf%x
 IDF(1)%IXV=1
  
 !## all
 IF(.NOT.PRESENT(XMIN))THEN
  IF(IDFOPEN(IDF(1)%IU,FNAME,'RO',1,1).AND. &
     IDFREADDIM(1,IDF(1)).AND. &
     IDFREADDATA(1,IDF(1)))THEN
  ELSE
   RETURN
  ENDIF
 !## window
 ELSE
  IF(.NOT.IDFREAD(IDF(1),FNAME,0))RETURN
  IF(.NOT.IDFREADPART(IDF(1),XMIN,YMIN,XMAX,YMAX))RETURN
 ENDIF
 CLOSE(IDF(1)%IU)
 
 !## get statistics
 CALL UTL_STDEF(IDF(1)%V,SIZE(IDF(1)%V),IDF(1)%NODATA,VAR,MEAN,NPOP)

 !## write statistics to file
 IF(IU.GT.0)THEN
  WRITE(LINE,'(I5,A1,I15,A1,2(F15.7,A1))')  I,',',NPOP,',',MEAN,',',VAR
  CALL INFOSTAT_PERC(IU,LINE)
  WRITE(IU,'(A)') TRIM(LINE) !'==='
  CALL IDFDEALLOCATE(IDF,1); DEALLOCATE(IDF); RETURN
 ENDIF
 
 CALL WDIALOGLOAD(ID_DINFOSTAT,ID_DINFOSTAT)
 CALL WDIALOGPUTIMAGE(ID_GRAPH,ID_ICONHISTOGRAM,1)
 CALL WDIALOGPUTIMAGE(ID_CALC,ID_ICONCALC,1)

 CALL WGRIDPUTCELLSTRING(IDF_GRID1,1,1,'SAMPLE')
 CALL WGRIDPUTCELLREAL  (IDF_GRID1,2,1,REAL(SIZE(IDF(1)%V)),'(F15.1)')
 CALL WGRIDPUTCELLSTRING(IDF_GRID1,1,2,'Population')
 CALL WGRIDPUTCELLREAL  (IDF_GRID1,2,2,REAL(NPOP),'(F15.1)')
 CALL WGRIDPUTCELLSTRING(IDF_GRID1,1,3,'Mean')
 CALL WGRIDPUTCELLREAL  (IDF_GRID1,2,3,MEAN,'(F15.7)')
 CALL WGRIDPUTCELLSTRING(IDF_GRID1,1,4,'Standard Deviation')
 CALL WGRIDPUTCELLREAL  (IDF_GRID1,2,4,VAR,'(F15.7)')
 CALL WGRIDPUTCELLSTRING(IDF_GRID1,1,5,'Sum')
 CALL WGRIDPUTCELLREAL  (IDF_GRID1,2,5,MEAN*REAL(NPOP),'(F15.7)')
 
 CALL INFOSTAT_PERC(IU,LINE)

 CALL WDIALOGSELECT(ID_DINFOSTAT)
 CALL WDIALOGSHOW(-1,-1,0,3)

 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)
   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (ID_CALC)
      CALL INFOSTAT_PERC(IU,LINE)
     CASE (ID_GRAPH)
      CALL INFOSTAT_PERC(IU,LINE)
      !## display graph
      CALL PROFILE_PLOTGRAPH('Percentile','Values (-)',.FALSE.)
     CASE (IDHELP)
       CALL IMODGETHELP('3.4.3','MMO.MapInfo')
     CASE (IDCANCEL)
      EXIT
    END SELECT
  END SELECT
 ENDDO

 CALL WDIALOGSELECT(ID_DINFOSTAT); CALL WDIALOGUNLOAD()

 CALL IDFDEALLOCATE(IDF,1); DEALLOCATE(IDF)
 CALL PROFILE_DEALLGRAPH()

 CALL WDIALOGSELECT(ID_DINFO)

 END SUBROUTINE INFOSTAT

 !###======================================================================
 SUBROUTINE INFOSTAT_PERC(IU,LINE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 CHARACTER(LEN=*),INTENT(INOUT) :: LINE
 INTEGER :: NPERC,I,J,NAJ
 REAL :: DPERC

 IF(IU.EQ.0)THEN
  CALL WDIALOGSELECT(ID_DINFOSTAT)
  CALL WDIALOGGETREAL(IDF_REAL1,DPERC)
  IF(DPERC.LE.0.0.OR.DPERC.GE.100.)THEN
   DPERC=10.0
   CALL WDIALOGPUTREAL(IDF_REAL1,DPERC)
  ENDIF
 ELSE
  DPERC=5.0
 ENDIF
  
 NPERC=INT(100.0/DPERC)
 IF(MOD(100.0,DPERC).EQ.0.0)NPERC=NPERC-1
 IF(ALLOCATED(GRAPH))CALL PROFILE_DEALLGRAPH()
 CALL PROFILE_ALLGRAPH(1,1)
 ALLOCATE(GRAPH(1,1)%RX(NPERC+2))
 ALLOCATE(GRAPH(1,1)%RY(NPERC+2))
 GRAPH(1,1)%RX(1)=DPERC
 DO I=2,NPERC
  GRAPH(1,1)%RX(I)=GRAPH(1,1)%RX(I-1)+DPERC
 END DO
 
 !## get percentiles
 CALL UTL_GETMED(IDF(1)%V,SIZE(IDF(1)%V),IDF(1)%NODATA,GRAPH(1,1)%RX,NPERC,NAJ,GRAPH(1,1)%RY)
 DO I=NPERC+1,2,-1
  GRAPH(1,1)%RX(I)=GRAPH(1,1)%RX(I-1)
  GRAPH(1,1)%RY(I)=GRAPH(1,1)%RY(I-1)
 END DO
 GRAPH(1,1)%RX(1)=0.0
 GRAPH(1,1)%RY(1)=IDF(1)%V(1)
! !## if not yet computed (100%)
 NPERC=NPERC+2
 GRAPH(1,1)%RX(NPERC)=100.0
 GRAPH(1,1)%RY(NPERC)=IDF(1)%NODATA
 IF(NAJ.GT.0)THEN
  GRAPH(1,1)%RY(NPERC)=IDF(1)%V(NAJ)
 ENDIF
 GRAPH(1,1)%NP=NPERC
 GRAPH(1,1)%GTYPE=2
 GRAPH(1,1)%LEGTXT='Value'
 GRAPH(1,1)%ICLR=WRGB(56,180,176)

 J=5

 IF(IU.EQ.0)THEN
  !## clear all
  DO I=1,WINFOGRID(IDF_GRID1,GRIDROWSMAX)
   CALL WGRIDCLEARCELL(IDF_GRID1,1,I+J)
   CALL WGRIDCLEARCELL(IDF_GRID1,2,I+J)
  ENDDO
 
  !## put percentiles into grid
  DO I=1,NPERC
   CALL WGRIDPUTCELLSTRING(IDF_GRID1,1,I+J,'Percentile '//TRIM(RTOS(GRAPH(1,1)%RX(I),'F',2)))
   CALL WGRIDPUTCELLREAL  (IDF_GRID1,2,I+J,GRAPH(1,1)%RY(I),'(F15.7)')
  END DO
 
 ELSE
  DO I=1,NPERC
   WRITE(LINE,'(A,A1,A16)') TRIM(LINE),',',TRIM(RTOS(GRAPH(1,1)%RY(I),'F',7))
!   LINE=TRIM(LINE)//','//TRIM(RTOS(GRAPH(1,1)%RX(I),'F',7))//')='//TRIM(RTOS(GRAPH(1,1)%RY(I),'F',7))
!   WRITE(IU,'(A)') 'Percentile('//TRIM(LINE) 
  ENDDO
 ENDIF
 
 END SUBROUTINE INFOSTAT_PERC

 !###======================================================================
 LOGICAL FUNCTION INFOADJUST(IOPTION)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IOPTION
 INTEGER :: IU,IOS,IPLOT,I
 REAL :: X1,Y1
 LOGICAL :: LEX
 CHARACTER(LEN=50) :: ERRTXT

 INFOADJUST=.FALSE.
 
 CALL WDIALOGGETMENU(IDF_MENU1,IPLOT)

 INQUIRE(FILE=MP(IPLOT)%IDFNAME,OPENED=LEX)
 IF(LEX)THEN
  INQUIRE(FILE=MP(IPLOT)%IDFNAME,NUMBER=IU)
  CLOSE(IU)
 ENDIF
 IF(IOPTION.EQ.1)THEN
  IU=UTL_GETUNIT()
  IF(ICF.EQ.0)THEN
   CALL OSD_OPEN(IU,FILE=MP(IPLOT)%IDFNAME,STATUS='OLD',ACCESS='DIRECT',RECL=4, &
       ACTION='READWRITE',FORM='UNFORMATTED',IOSTAT=IOS)
  ELSEIF(ICF.EQ.1)THEN
   CALL OSD_OPEN(IU,FILE=MP(IPLOT)%IDFNAME,STATUS='OLD',ACCESS='DIRECT',RECL=1, &
       ACTION='READWRITE',FORM='UNFORMATTED',IOSTAT=IOS)
  ENDIF
  IF(IOS.NE.0)THEN
   CALL OSD_IOSTAT_MSG(IOS,ERRTXT)
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not change the IDF due to writing permissions'//CHAR(13)// &
         TRIM(ERRTXT),'Error')
   RETURN
  ELSE
   X1=MP(IPLOT)%IDF%XMIN; Y1=MP(IPLOT)%IDF%YMIN   
   CALL WDIALOGGETREAL(ID_XMIN,MP(IPLOT)%IDF%XMIN)
   CALL WDIALOGGETREAL(ID_YMIN,MP(IPLOT)%IDF%YMIN)
   X1=MP(IPLOT)%IDF%XMIN-X1; Y1=MP(IPLOT)%IDF%YMIN-Y1
   MP(IPLOT)%IDF%XMAX=MP(IPLOT)%IDF%XMAX+X1
   MP(IPLOT)%IDF%YMAX=MP(IPLOT)%IDF%YMAX+Y1
   CALL WDIALOGGETREAL(ID_NODATA,MP(IPLOT)%IDF%NODATA)
   CALL WDIALOGGETMENU(IDF_MENU2,MP(IPLOT)%UNITS)
   MP(IPLOT)%UNITS=MP(IPLOT)%UNITS-1
   WRITE(IU,REC=3+ICF)  MP(IPLOT)%IDF%XMIN
   WRITE(IU,REC=4+ICF)  MP(IPLOT)%IDF%XMAX
   WRITE(IU,REC=5+ICF)  MP(IPLOT)%IDF%YMIN
   WRITE(IU,REC=6+ICF)  MP(IPLOT)%IDF%YMAX
   WRITE(IU,REC=9+ICF)  MP(IPLOT)%IDF%NODATA
  ENDIF
  CLOSE(IU)
 ELSEIF(IOPTION.EQ.2)THEN
  !## read entire IDF
  ALLOCATE(IDF(1)); DO I=1,SIZE(IDF); CALL IDFNULLIFY(IDF(I)); ENDDO
  IF(IDFREAD(IDF(1),MP(IPLOT)%IDFNAME,1))THEN  
   CALL WDIALOGGETREAL(IDF_REAL1,IDF(1)%TOP); CALL WDIALOGGETREAL(IDF_REAL2,IDF(1)%BOT)
   IF(IDF(1)%TOP.LE.IDF(1)%BOT)THEN; IDF(1)%ITB=0
   ELSE; IDF(1)%ITB=1; ENDIF  
   IF(.NOT.IDFWRITE(IDF(1),MP(IPLOT)%IDFNAME,1))THEN; RETURN; ENDIF; CALL IDFDEALLOCATE(IDF,1); DEALLOCATE(IDF)
  ELSE
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not change the IDF.','Error')
   RETURN
  ENDIF   
 ENDIF

 INFOADJUST=.TRUE.
 
 END FUNCTION INFOADJUST

 !###======================================================================
 SUBROUTINE INFOFILL()
 !###======================================================================
 IMPLICIT NONE
 INTEGER,PARAMETER :: MAXIDS1=13
 INTEGER,PARAMETER :: MAXIDS2=15
 INTEGER :: I,J,IPLOT,N
 REAL :: MINCS,MAXCS
 INTEGER,DIMENSION(MAXIDS1) :: IDS1
 INTEGER,DIMENSION(MAXIDS2) :: IDS2
 CHARACTER(LEN=256) :: FNAME
 LOGICAL :: LEX
 DATA IDS1/IDF_LABEL8,IDF_LABEL16,IDF_LABEL25,IDF_LABEL26,IDF_LABEL27,IDF_LABEL28,IDF_LABEL5,IDF_LABEL14, &
           IDF_LABEL11,IDF_LABEL24,ID_ADJUST,IDF_LABEL15,IDF_LABEL10/
 DATA IDS2/ID_DXMIN,ID_DXMAX,ID_DYMIN,ID_DYMAX,IDF_REAL1,IDF_REAL2,ID_ZMIN,ID_ZMAX,ID_DZ,ID_NODATA,IDF_MENU2,&
           ID_ZMIN2,ID_ZMAX2,ID_DZ2,IDF_LABEL15/

 CALL WDIALOGGETMENU(IDF_MENU1,IPLOT)

 CALL WDIALOGFIELDSTATE(IDF_STRING4,1)
 CALL WDIALOGPUTSTRING(IDF_STRING4,MP(IPLOT)%ALIAS)
 
 J=3
 IF(MP(IPLOT)%IPLOT.EQ.1.OR.MP(IPLOT)%IPLOT.EQ.5)J=1
 DO I=1,MAXIDS1; CALL WDIALOGFIELDSTATE(IDS1(I),J); END DO
 J=3
 IF(MP(IPLOT)%IPLOT.EQ.1.OR.MP(IPLOT)%IPLOT.EQ.5)J=2
 DO I=1,MAXIDS2; CALL WDIALOGFIELDSTATE(IDS2(I),J); END DO
 
 IF(MP(IPLOT)%IPLOT.EQ.1.OR.MP(IPLOT)%IPLOT.EQ.5)THEN
  CALL WDIALOGFIELDSTATE(ID_STAT,1)
 ELSE
  CALL WDIALOGFIELDSTATE(ID_STAT,3)
 ENDIF
 
 IF(MP(IPLOT)%IPLOT.EQ.1.OR.MP(IPLOT)%IPLOT.EQ.5)THEN

  !## get idf for mdf file
  LEX=.TRUE.
  IF(MP(IPLOT)%IPLOT.EQ.5)THEN
   FNAME=MP(IPLOT)%IDFNAME
   !## read *.mdf file, only to get selected idf to be plotted
   IF(READMDF(MP(IPLOT)%IDFNAME,N))THEN
    MP(IPLOT)%IDFNAME=MDF(MP(IPLOT)%NLIDF)%FNAME
    CALL MDFDEALLOCATE()
   ENDIF
   CALL WDIALOGPUTSTRING(IDF_LABEL30,'Selected:')
  ELSE
   CALL WDIALOGPUTSTRING(IDF_LABEL30,'Fullname:')
  ENDIF
  CALL WDIALOGFIELDSTATE(ID_INFO,1)

  CALL WDIALOGPUTSTRING(IDF_STRING2,TRIM(MP(IPLOT)%IDFNAME))

  IF(LEX)THEN

   !## re(read) idf dimensions
   LEX=IDFREAD(MP(IPLOT)%IDF,MP(IPLOT)%IDFNAME,0)

   !## place comments, if found
   IF(.NOT.ASSOCIATED(MP(IPLOT)%IDF%COMMENT))THEN
    CALL WDIALOGPUTSTRING(IDF_STRING3,'No additional information found')
   ELSE
    STRING=''
    DO I=1,SIZE(MP(IPLOT)%IDF%COMMENT)
     IF(I.EQ.1)THEN
      STRING=MP(IPLOT)%IDF%COMMENT(I)
     ELSE
      STRING=STRING(1:(I-1)*4)//MP(IPLOT)%IDF%COMMENT(I)
     ENDIF
    ENDDO
    CALL WDIALOGPUTSTRING(IDF_STRING3,TRIM(STRING))    
   ENDIF 
   
   CALL WDIALOGPUTSTRING(IDF_LABEL14,'') !## units
   CALL WDIALOGPUTSTRING(IDF_LABEL15,'') !## units
   
   CALL WDIALOGPUTSTRING(ID_TXT1,'Map Size: '//TRIM(ITOS(MP(IPLOT)%IDF%NCOL))//' columns x '// &
                                               TRIM(ITOS(MP(IPLOT)%IDF%NROW))//' rows')

   CALL WDIALOGPUTREAL(ID_XMIN,MP(IPLOT)%IDF%XMIN,'(G15.7)')
   CALL WDIALOGPUTREAL(ID_XMAX,MP(IPLOT)%IDF%XMAX,'(G15.7)')
   CALL WDIALOGPUTREAL(ID_DX,MP(IPLOT)%IDF%XMAX-MP(IPLOT)%IDF%XMIN,'(G15.7)')
   CALL WDIALOGPUTREAL(ID_YMIN,MP(IPLOT)%IDF%YMIN,'(G15.7)')
   CALL WDIALOGPUTREAL(ID_YMAX,MP(IPLOT)%IDF%YMAX,'(G15.7)')
   CALL WDIALOGPUTREAL(ID_DY,MP(IPLOT)%IDF%YMAX-MP(IPLOT)%IDF%YMIN,'(G15.7)')

   CALL WDIALOGPUTOPTION(IDF_MENU2,MP(IPLOT)%UNITS+1)
   CALL WDIALOGPUTREAL(ID_NODATA,MP(IPLOT)%IDF%NODATA,'(G15.7)')

   CALL WDIALOGPUTREAL(ID_ZMIN,MP(IPLOT)%IDF%DMIN,'(G15.7)')
   CALL WDIALOGPUTREAL(ID_ZMAX,MP(IPLOT)%IDF%DMAX,'(G15.7)')
   CALL WDIALOGPUTREAL(ID_DZ,MP(IPLOT)%IDF%DMAX-MP(IPLOT)%IDF%DMIN,'(G15.7)')

   CALL WDIALOGPUTREAL(ID_ZMIN2,MP(IPLOT)%UMIN,'(G15.7)')
   CALL WDIALOGPUTREAL(ID_ZMAX2,MP(IPLOT)%UMAX,'(G15.7)')
   CALL WDIALOGPUTREAL(ID_DZ2,MP(IPLOT)%UMAX-MP(IPLOT)%UMIN,'(G15.7)')

   IF(MP(IPLOT)%IDF%IEQ.EQ.0)THEN
    CALL WDIALOGPUTREAL(ID_DXMIN,MP(IPLOT)%IDF%DX,'(G15.7)')
    CALL WDIALOGPUTREAL(ID_DYMIN,MP(IPLOT)%IDF%DY,'(G15.7)')
    CALL WDIALOGFIELDSTATE(ID_DYMAX,3)
    CALL WDIALOGFIELDSTATE(ID_DXMAX,3)
   ELSE
    MINCS=MP(IPLOT)%IDF%XMAX-MP(IPLOT)%IDF%XMIN
    MAXCS=0.0
    DO I=1,MP(IPLOT)%IDF%NCOL
     MINCS=MIN(MINCS,MP(IPLOT)%IDF%SX(I)-MP(IPLOT)%IDF%SX(I-1))
     MAXCS=MAX(MAXCS,MP(IPLOT)%IDF%SX(I)-MP(IPLOT)%IDF%SX(I-1))
    END DO
    CALL WDIALOGPUTREAL(ID_DXMIN,MINCS,'(G15.7)')
    CALL WDIALOGPUTREAL(ID_DXMAX,MAXCS,'(G15.7)')
    MINCS=MP(IPLOT)%IDF%YMAX-MP(IPLOT)%IDF%YMIN
    MAXCS=0.0
    DO I=1,MP(IPLOT)%IDF%NROW
     MINCS=MIN(MINCS,MP(IPLOT)%IDF%SY(I-1)-MP(IPLOT)%IDF%SY(I))
     MAXCS=MAX(MAXCS,MP(IPLOT)%IDF%SY(I-1)-MP(IPLOT)%IDF%SY(I))
    END DO
    CALL WDIALOGFIELDSTATE(ID_DYMAX,2)
    CALL WDIALOGFIELDSTATE(ID_DXMAX,2)
    CALL WDIALOGPUTREAL(ID_DYMIN,MINCS,'(G15.7)')
    CALL WDIALOGPUTREAL(ID_DYMAX,MAXCS,'(G15.7)')
   ENDIF

   CALL WDIALOGFIELDSTATE(ID_ADJUSTTB,1)

   IF(MP(IPLOT)%IDF%ITB.EQ.0)THEN
    CALL WDIALOGFIELDSTATE(IDF_REAL1,3)
    CALL WDIALOGFIELDSTATE(IDF_REAL2,3)
    CALL WDIALOGFIELDSTATE(IDF_LABEL25,2)
    CALL WDIALOGFIELDSTATE(IDF_LABEL26,2)
    CALL WDIALOGFIELDSTATE(IDF_LABEL27,2)
    CALL WDIALOGFIELDSTATE(IDF_LABEL28,2)
   ELSE
    CALL WDIALOGFIELDSTATE(IDF_REAL1,2)
    CALL WDIALOGFIELDSTATE(IDF_REAL2,2)
    CALL WDIALOGPUTREAL(IDF_REAL1,MP(IPLOT)%IDF%TOP,'(G15.7)')
    CALL WDIALOGPUTREAL(IDF_REAL2,MP(IPLOT)%IDF%BOT,'(G15.7)')
    CALL WDIALOGFIELDSTATE(IDF_LABEL25,1)
    CALL WDIALOGFIELDSTATE(IDF_LABEL26,1)
    CALL WDIALOGFIELDSTATE(IDF_LABEL27,1)
    CALL WDIALOGFIELDSTATE(IDF_LABEL28,1)
   ENDIF

   !## deallocate idf%x
   CALL IDFDEALLOCATEX(MP(IPLOT)%IDF)
   CLOSE(MP(IPLOT)%IDF%IU); MP(IPLOT)%IDF%IU=0
  ENDIF
  IF(MP(IPLOT)%IPLOT.EQ.5)MP(IPLOT)%IDFNAME=FNAME
  CALL WDIALOGFIELDSTATE(ID_EDIT,1)

 !## ipf's/iff's/gen's
 ELSE

  CALL WDIALOGPUTSTRING(IDF_LABEL30,'Fullname:')
  IF(MP(IPLOT)%IPLOT.EQ.6)THEN !## genfile to be used for datagrid plotting
   CALL WDIALOGFIELDSTATE(ID_INFO,1)
  ELSE
   CALL WDIALOGFIELDSTATE(ID_INFO,2)
  ENDIF
  CALL WDIALOGPUTSTRING(IDF_STRING2,TRIM(MP(IPLOT)%IDFNAME))
  CALL WDIALOGFIELDSTATE(ID_EDIT,0)

  CALL WDIALOGPUTREAL(ID_XMIN,MP(IPLOT)%XMIN,'(G15.7)')
  CALL WDIALOGPUTREAL(ID_XMAX,MP(IPLOT)%XMAX,'(G15.7)')
  CALL WDIALOGPUTREAL(ID_DX,MP(IPLOT)%XMAX-MP(IPLOT)%XMIN,'(G15.7)')
  CALL WDIALOGPUTREAL(ID_YMIN,MP(IPLOT)%YMIN,'(G15.7)')
  CALL WDIALOGPUTREAL(ID_YMAX,MP(IPLOT)%YMAX,'(G15.7)')
  CALL WDIALOGPUTREAL(ID_DY,MP(IPLOT)%YMAX-MP(IPLOT)%YMIN,'(G15.7)')

  CALL WDIALOGFIELDSTATE(ID_ADJUST,3)
  CALL WDIALOGFIELDSTATE(ID_ADJUSTTB,3)

  IF(MP(IPLOT)%IPLOT.EQ.2)CALL WDIALOGPUTSTRING(ID_TXT1,'IPF File')
  IF(MP(IPLOT)%IPLOT.EQ.3)CALL WDIALOGPUTSTRING(ID_TXT1,'IFF File')
  IF(MP(IPLOT)%IPLOT.EQ.4)CALL WDIALOGPUTSTRING(ID_TXT1,'ISG File')
  IF(MP(IPLOT)%IPLOT.EQ.6)CALL WDIALOGPUTSTRING(ID_TXT1,'GEN File')
  
  CALL WDIALOGPUTSTRING(IDF_STRING3,'No additional information found')
  
  IF(MP(IPLOT)%ILEG.EQ.1)THEN
   CALL WDIALOGFIELDSTATE(ID_ZMIN,1)
   CALL WDIALOGFIELDSTATE(ID_ZMAX,1)
   CALL WDIALOGFIELDSTATE(ID_DZ  ,1)
   CALL WDIALOGFIELDSTATE(IDF_LABEL5,1)
   CALL WDIALOGPUTREAL(ID_ZMIN,MP(IPLOT)%UMIN,'(G15.7)')
   CALL WDIALOGPUTREAL(ID_ZMAX,MP(IPLOT)%UMAX,'(G15.7)')
   CALL WDIALOGPUTREAL(ID_DZ,MP(IPLOT)%UMAX-MP(IPLOT)%UMIN,'(G15.7)')
  ENDIF

 ENDIF
  
 END SUBROUTINE INFOFILL

 !###======================================================================
 SUBROUTINE INFOMETA()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,IU,IWIN,IPLOT
 LOGICAL :: LEX

 CALL WDIALOGGETMENU(IDF_MENU1,IPLOT)

 CALL WINDOWOPENCHILD(IWIN,FLAGS=SYSMENUON+OWNEDBYPARENT,WIDTH=500,HEIGHT=400)
 I=INDEXNOCASE(MP(IPLOT)%IDFNAME,'.IDF',.TRUE.)
 INQUIRE(FILE=MP(IPLOT)%IDFNAME(1:I)//'MET',EXIST=LEX)
 IF(.NOT.LEX)THEN
  IU=UTL_GETUNIT()
  CALL OSD_OPEN(IU,FILE=MP(IPLOT)%IDFNAME(1:I)//'MET',STATUS='UNKNOWN',FORM='FORMATTED')
  WRITE(IU,*) '# Algemene informatie'
  WRITE(IU,*) '- Bestandsnaam      : '//TRIM(MP(IPLOT)%IDFNAME)
  WRITE(IU,*) '- Locatie bestand   : '
  WRITE(IU,*) '- Publicatie datum  : '
  WRITE(IU,*) '- Versienr bestand  : '
  WRITE(IU,*) '- Versienr model    : '
  WRITE(IU,*) '- Beschrijving      : '
  WRITE(IU,*) '- Producent         : '
  WRITE(IU,*) '- Type              : '
  WRITE(IU,*) ''
  WRITE(IU,*) '# Beschrijving dataset'
  WRITE(IU,*) '- Eenheid           : '
  WRITE(IU,*) '- Resolutie         : '
  WRITE(IU,*) '- Herkomst/Bron     : '
  WRITE(IU,*) '- Procesbeschrijving: '
  WRITE(IU,*) '- Toepassingsschaal : '
  WRITE(IU,*) ''
  WRITE(IU,*) '# Administratie'
  WRITE(IU,*) '- Organisatie       : '
  WRITE(IU,*) '- Website           : '
  WRITE(IU,*) '- Contactpersoon    : '
  WRITE(IU,*) '- E-mail adres      : '
  CLOSE(IU)
 ENDIF
 CALL WEDITFILE(MP(IPLOT)%IDFNAME(1:I)//'MET',ITYPE=MODAL,IDMENU=0, &
                IFLAGS=MUSTEXIST+WORDWRAP+NOFILENEWOPEN+NOFILESAVEAS,IFONT=COURIERNEW)

 END SUBROUTINE INFOMETA

END MODULE MOD_INFO
