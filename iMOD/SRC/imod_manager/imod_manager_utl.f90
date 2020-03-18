!!  Copyright (C) Stichting Deltares, 2005-2019.
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
MODULE MOD_MANAGER_UTL

USE WINTERACTER
USE RESOURCE
USE MODPLOT
USE MOD_PREF_PAR
USE MOD_COLOURS
USE MOD_NC2IDF, ONLY : INETCDF
USE MOD_UTL 
USE MOD_MDF
USE MOD_IFF, ONLY : IFFGETUNIT
USE MOD_GEF2IPF_PAR, ONLY : GEFNAMES,IPFFNAME
USE MOD_IDF
USE MOD_LEGEND 
USE MOD_ISG_PAR, ONLY : MAXFILES
USE MOD_GEF2IPF, ONLY : GEF2IPF_MAIN
USE MOD_ASC2IDF, ONLY : ASC2IDF_IMPORTASC,ASC2IDF_IMPORTASC_TYPE5
USE MOD_NC2IDF, ONLY : NC2IDF_IMPORTNC,INETCDF
USE MOD_ISG_UTL, ONLY : UTL_GETUNITSISG
USE MOD_TOPO, ONLY : TOPO1UPDATEMANAGER 
USE MOD_MAP2IDF, ONLY : MAP2IDF_IMPORTMAP

CONTAINS
 
 !###======================================================================
 SUBROUTINE MANAGER_UTL_ADDFILE(IDFNAMEGIVEN,LEGNAME,ISTYLE,LDEACTIVATE,IPFICOL,ILABELS,IPFASSFILES) 
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: IDFNAMEGIVEN
 CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: LEGNAME
 LOGICAL,INTENT(IN),OPTIONAL :: LDEACTIVATE
 INTEGER,INTENT(IN),OPTIONAL :: ISTYLE
 INTEGER,INTENT(IN),OPTIONAL,DIMENSION(:) :: IPFASSFILES
 INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: ILABELS
 INTEGER,DIMENSION(5),INTENT(IN),OPTIONAL :: IPFICOL
 INTEGER :: IDF,NIDF,I,J,K,IOS,IPLOT,N,IACT,M
 INTEGER,ALLOCATABLE,DIMENSION(:) :: ILIST
 INTEGER,DIMENSION(MAXFILES) :: IU
 REAL(KIND=DP_KIND) :: DR
 CHARACTER(LEN=10000) :: IDFNAME,IDFLIST
 CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: FNAMES  ! list of file in case multi-file selection mode
 LOGICAL :: LLEG,LGEF

 CALL WINDOWSELECT(0)
 IF(WMENUGETSTATE(ID_3DTOOL,2).EQ.1)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You cannot add files to the iMOD Manager once the 3D Tool is active.'//CHAR(13)// &
   'Close the 3D Tool first','Information')
  RETURN
 ENDIF

 !## how many active before opening files
 IACT=MPW%NACT

 !## initialize iu
 IU=0

 IF(.NOT.PRESENT(IDFNAMEGIVEN))THEN
  IDFNAME=''
  IF(INETCDF.EQ.0)THEN
   IF(.NOT.UTL_WSELECTFILE('All Known Files (*.idf;*.mdf;*.ipf;*.isg;*.iff;*.asc;*.gen;*.gef;*.map;*.csv)'//&
                    '|*.idf;*.mdf;*.ipf;*.isg;*.iff;*.arr;*.asc;*.gen;*.gef;*.map;*.csv|'// &
                    'iMOD Map (*.idf)|*.idf|'               //&
                    'iMOD Multi Data File (*.mdf)|*.mdf|'   //&
                    'iMOD Pointers (*.ipf)|*.ipf|'          //&
                    'iMOD Segment-River File (*.isg)|*.isg|'//&
                    'iMOD Flowline File (*.iff)|*.iff|'     //&
                    'iMOD Array File (*.arr)|*.arr|'        //&
                    'ESRI Raster file (*.asc)|*.asc|'       //&
                    'ESRI/iMOD GEN file (*.gen)|*.gen|'   //&
                    'GEF file (*.gef)|*.gef|'               //&
                    'PC Raster Map file (*.map)|*.map|'     //&
                    'Comma Seperated File (*.csv)|*.csv|',      &
                    LOADDIALOG+MUSTEXIST+PROMPTON+DIRCHANGE+MULTIFILE,IDFNAME,&
                    'Load iMOD Map (*.idf,*.mdf,*.ipf,*.isg,*.iff,*.asc,*.gen,*.gef,*.map,*.csv)'))RETURN
  ELSEIF(INETCDF.EQ.1)THEN
   IF(.NOT.UTL_WSELECTFILE('All Known Files (*.idf;*.mdf;*.ipf;*.isg;*.iff;*.nc;*.asc;*.gen;*.gef;*.map;*.csv)'//&
                    '|*.idf;*.mdf;*.ipf;*.isg;*.iff,*.arr;*.nc;*.asc;*.gen;*.gef;*.map;*.csv|'// &
                    'iMOD Map (*.idf)|*.idf|'               //&
                    'iMOD Multi Data File (*.mdf)|*.mdf|'   //&
                    'iMOD Pointers (*.ipf)|*.ipf|'          //&
                    'iMOD Segment-River File (*.isg)|*.isg|'//&
                    'iMOD Flowline File (*.iff)|*.iff|'     //&
                    'iMOD Array File (*.arr)|*.arr|'        //&
                    'NetCDF File (*.nc)|*.nc|'              //&
                    'ESRI Raster file (*.asc)|*.asc|'       //&
                    'ESRI/iMOD GEN file (*.gen)|*.gen|'   //&
                    'GEF file (*.gef)|*.gef|'               //&
                    'PC Raster Map file (*.map)|*.map|'    //&
                    'Comma Seperated File (*.csv)|*.csv|',      &
                    LOADDIALOG+MUSTEXIST+PROMPTON+DIRCHANGE+MULTIFILE,IDFNAME,&
                    'Load iMOD Map (*.idf,*.mdf,*.ipf,*.isg,*.iff,*.nc,*.asc,*.gen,*.gef,*.map,*.csv)'))RETURN
  ENDIF
 ELSE
  IDFNAME=IDFNAMEGIVEN
 ENDIF

 CALL UTL_MESSAGEHANDLE(0)

 K=INDEX(IDFNAME,CHAR(0))
 !## count number of iMOD files selected from the window (multi-file selection mode)
 IF(K.GT.0)THEN
  IDFLIST=IDFNAME
  NIDF=0
  I=K+1
  DO WHILE(.TRUE.)
   J=INDEX(IDFLIST(I:),CHAR(0))
   NIDF=NIDF+1
   IF(J.EQ.0)EXIT
   I=I+J
  END DO
 ELSE
  NIDF=1
 ENDIF

 !## inactivate all
 IF(PRESENT(LDEACTIVATE))THEN
  IF(LDEACTIVATE)MP%ISEL=.NOT.LDEACTIVATE
 ELSE
  MP%ISEL=.FALSE.
 ENDIF

 !## convert single variable after multi file selection to file list construction
 ALLOCATE(FNAMES(NIDF))
 DO IDF=1,NIDF
  IF(NIDF.GT.1)THEN
   I=INDEX(IDFLIST,CHAR(0))+1
   DO K=1,IDF-1
    J=INDEX(IDFLIST(I:),CHAR(0))
    I=I+J
   END DO
   J=INDEX(IDFLIST(I:),CHAR(0))
   K=INDEX(IDFLIST,CHAR(0))-1
   IF(J.EQ.0)THEN
    FNAMES(IDF)=IDFLIST(:K)//'\'//IDFLIST(I:)
   ELSE
    J=J+I
    FNAMES(IDF)=IDFLIST(:K)//'\'//IDFLIST(I:J-1)
   ENDIF
   J=INDEXNOCASE(FNAMES(IDF),CHAR(0),.TRUE.)
   IF(J.GT.0)FNAMES(IDF)=FNAMES(IDF)(:J-1)
  ELSE
   FNAMES(IDF)=IDFNAME
  ENDIF
  FNAMES(IDF)=UTL_CAP(FNAMES(IDF),'U')
 ENDDO

 !## Open iMOD files from CSV file, fill list "FNAMES" from given csv-file
 I=INDEXNOCASE(IDFNAME,'.',.TRUE.)+1
 IF(UTL_CAP(IDFNAME(I:I+2),'U').EQ.'CSV') CALL READCSV(IDFNAME,FNAMES,NIDF)
  
 LGEF=.FALSE.
 !## Process list of files to be loaded to the iMOD Manager
 DO IDF=1,NIDF

  IDFNAME=FNAMES(IDF)
  !## make sure \\-symbols are gone
  DO
   IF(INDEX(IDFNAME,'\\').EQ.0)EXIT
   IDFNAME=UTL_SUBST(IDFNAME,'\\','\')
  ENDDO
  
  !## check whether file already opened ... overwrite it otherwise
  DO IPLOT=1,MXMPLOT
   IF(MP(IPLOT)%IACT.AND.TRIM(UTL_CAP(MP(IPLOT)%IDFNAME,'U')).EQ.TRIM(UTL_CAP(IDFNAME,'U')))EXIT
  END DO
  !## get empty iplot-location
  IF(IPLOT.GT.MXMPLOT)THEN
   DO IPLOT=1,MXMPLOT
    IF(.NOT.MP(IPLOT)%IACT)EXIT
   END DO
  ENDIF
  IF(IPLOT.GT.MXMPLOT)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot open more than '//TRIM(ITOS(MXMPLOT))//' file in the iMOD Manager!','Error')
   EXIT
  ENDIF
 
  !## determine what kind of file *.idf, *.ipf etc.
  I=INDEXNOCASE(IDFNAME,'.',.TRUE.)+1
  SELECT CASE (IDFNAME(I:I+2))
   CASE ('IDF')
    MP(IPLOT)%IPLOT=1
   CASE ('IPF')
    MP(IPLOT)%IPLOT=2
   CASE ('GEF')
    LGEF=.TRUE.; MP(IPLOT)%IPLOT=0
    IF(UTL_WSELECTFILE('iMOD Point file (*.ipf)|*.ipf|',SAVEDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,&
       IPFFNAME,'Save GEF files in a single IPF file (*.ipf)'))THEN
     ALLOCATE(GEFNAMES(NIDF)); GEFNAMES=FNAMES; CALL GEF2IPF_MAIN(0,0); DEALLOCATE(GEFNAMES)
     IDFNAME=IPFFNAME; MP(IPLOT)%IPLOT=2
    ENDIF
   CASE ('ASC','NC ','MAP','ARR')
    IF(IDFNAME(I:I+2).EQ.'ASC')THEN
     CALL ASC2IDF_IMPORTASC(IDFNAME,0.0D0,0.0D0,I,0)
    ELSEIF(IDFNAME(I:I+2).EQ.'NC ')THEN
     CALL NC2IDF_IMPORTNC(IDFNAME,I)
    ELSEIF(IDFNAME(I:I+2).EQ.'MAP')THEN
     CALL MAP2IDF_IMPORTMAP(IDFNAME,I)
    ELSEIF(IDFNAME(I:I+2).EQ.'ARR')THEN
     CALL ASC2IDF_IMPORTASC_TYPE5(IDFNAME,I)
    ENDIF
    IF(I.EQ.0)THEN
     I=INDEXNOCASE(IDFNAME,'.',.TRUE.)
     IDFNAME=IDFNAME(:I)//'IDF'
     !## Re-check whether file already opened ... overwrite it otherwise
     DO I=1,MXMPLOT; IF(MP(I)%IACT.AND.MP(I)%IDFNAME.EQ.IDFNAME)EXIT; END DO
     IF(I.LE.MXMPLOT)IPLOT=I
     MP(IPLOT)%IPLOT=1
    ELSE
     MP(IPLOT)%IPLOT=0
     EXIT
    ENDIF
   CASE ('IFF')
    MP(IPLOT)%IPLOT=3
   CASE ('ISG')
    MP(IPLOT)%IPLOT=4
   CASE ('MDF')
    MP(IPLOT)%IPLOT=5
   CASE ('GEN')
    MP(IPLOT)%IPLOT=6
   CASE DEFAULT
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot recognize current extension in'//CHAR(13)//TRIM(IDFNAME),'Error')
    EXIT
  END SELECT

  !## open idf-file (only to test whether file exists!)
  IF(MP(IPLOT)%IPLOT.EQ.1)THEN
   IF(.NOT.IDFREAD(MP(IPLOT)%IDF,IDFNAME,0))MP(IPLOT)%IPLOT=0
   IU(1)=MP(IPLOT)%IDF%IU
  !## open ipf-file
  ELSEIF(MP(IPLOT)%IPLOT.EQ.2)THEN
   IU(1)=UTL_GETUNITIPF(IDFNAME,'OLD'); IF(IU(1).LE.0)MP(IPLOT)%IPLOT=0
  !## open iff-file
  ELSEIF(MP(IPLOT)%IPLOT.EQ.3)THEN
   IU(1)=IFFGETUNIT(IDFNAME,'OLD'); IF(IU(1).LE.0)MP(IPLOT)%IPLOT=0
  !## open isg-file
  ELSEIF(MP(IPLOT)%IPLOT.EQ.4)THEN
   CALL UTL_GETUNITSISG(IU,IDFNAME,'OLD'); IF(MINVAL(IU).LE.0)MP(IPLOT)%IPLOT=0
  !## open mdf-file
  ELSEIF(MP(IPLOT)%IPLOT.EQ.5)THEN
   IF(.NOT.READMDF(IDFNAME,N))MP(IPLOT)%IPLOT=0; IF(N.EQ.0)MP(IPLOT)%IPLOT=0
   MP(IPLOT)%NLIDF=1 !## default take the first to be plotted
  !## open gen-file
  ELSEIF(MP(IPLOT)%IPLOT.EQ.6)THEN
   IU(1)=UTL_GETUNIT()
   CALL OSD_OPEN(IU(1),FILE=IDFNAME,STATUS='OLD',FORM='FORMATTED',ACTION='READ,DENYWRITE',ACCESS='SEQUENTIAL',IOSTAT=IOS)
   IF(IOS.NE.0)MP(IPLOT)%IPLOT=0
  ENDIF

  IF(MP(IPLOT)%IPLOT.EQ.0)EXIT !## idfloop

  !## initialize plot-variables
  MPW%NACT            =MPW%NACT+1
  MP(IPLOT)%IACT      =.TRUE.               !## plot active
  MP(IPLOT)%ISEL      =.TRUE.               !## selected
  MP(IPLOT)%IDFNAME   =IDFNAME              !## name of the idf-file
  I=INDEX(IDFNAME,'.')-1
  J=INDEXNOCASE(IDFNAME,'\',.TRUE.)+1
 
  LLEG=.TRUE.
  IF(PRESENT(LEGNAME))THEN
   !## do not generate a legend
   IF(LEGNAME.NE.'')LLEG=.FALSE.
  ENDIF

  MP(IPLOT)%ALIAS=IDFNAME(J:)

  I=MOD(IPLOT,MAXCOLOUR); I=MAX(1,I)
  MP(IPLOT)%SCOLOR=ICOLOR(I)   !## color for profile
  !## active - drawing lines in profile on default
  CALL UTL_READARRAY((/1,1,0,0,0,0/),6,MP(IPLOT)%PRFTYPE) 

  MP(IPLOT)%IDFI =0
  MP(IPLOT)%UNITS=0   

  SELECT CASE (MP(IPLOT)%IPLOT)

   !## idf/udf
   CASE (1,7)

    MP(IPLOT)%THICKNESS=1  !## contour width
    IF(PRESENT(ISTYLE))THEN
     MP(IPLOT)%IDFKIND=ISTYLE
    ELSE
     CALL UTL_READARRAY((/1,0,0/),3,MP(IPLOT)%IDFKIND)
    ENDIF

   !## ipf
   CASE (2)

    !## active/non active in profile
    MP(IPLOT)%PRFTYPE=1    
    IF(PRESENT(ILABELS))THEN
     READ(IU(1),*); READ(IU(1),*) M
     ALLOCATE(ILIST(M)); ILIST=0; DO I=1,SIZE(ILABELS); ILIST(ILABELS(I))=1; ENDDO  
     CALL UTL_READARRAY(ILIST,M,MP(IPLOT)%IEQ); DEALLOCATE(ILIST)
     !## no value plotted <--- used as binaire pointer for label plotting, white
     MP(IPLOT)%IEQ=-1.0D0*MP(IPLOT)%IEQ    
    ELSE
     MP(IPLOT)%IEQ=0    !no value plotted <--- used as binaire pointer for label plotting
    ENDIF  
    IF(PRESENT(ISTYLE))THEN
     IF(.NOT.LLEG)THEN !PRESENT(LEGNAME))THEN
      MP(IPLOT)%ILEG   =ISTYLE    !legend used for plotting
      IF(ISTYLE.EQ.1)THEN
       IF(PRESENT(IPFICOL))THEN
        MP(IPLOT)%IATTRIB=IPFICOL(3)
       ELSE
        MP(IPLOT)%IATTRIB=3    !initial first label for colouring
       ENDIF
      ENDIF
     ELSE
      MP(IPLOT)%ILEG   =0    !no legend used for plotting, use colour in %scolor
      MP(IPLOT)%ILEGDLF=1    
      MP(IPLOT)%IATTRIB=1    !initial first label for colouring
     ENDIF
    ELSE
     MP(IPLOT)%ILEG   =0    !no legend used for plotting, use colour in %scolor
     MP(IPLOT)%ILEGDLF=1    
     MP(IPLOT)%IATTRIB=1    !initial first label for colouring
    ENDIF
    !## plot associated files
    MP(IPLOT)%ASSFILES=0
    IF(PRESENT(IPFASSFILES))THEN
     MP(IPLOT)%ASSFILES(1)=IPFASSFILES(1)
     MP(IPLOT)%ASSFILES(2)=IPFASSFILES(2)
    ENDIF
    MP(IPLOT)%IDFKIND=0    !type of plotting for associate file
    MP(IPLOT)%IDFI   =250  !sight (m)
    MP(IPLOT)%SCOLOR =WRGB(100,100,100)! single - colour    !no colouring, attribute colouring
    MP(IPLOT)%UNITS  =1    !plot associated files within ipf profile mode
    IF(PRESENT(IPFICOL))THEN
     MP(IPLOT)%XCOL  =IPFICOL(1)    !column used for X-COORDINATE
     MP(IPLOT)%YCOL  =IPFICOL(2)    !column used for Y-COORDINATE
     MP(IPLOT)%HCOL  =IPFICOL(4)    !column used for HIGHLIGHTING
     MP(IPLOT)%HCOL_METHOD=IPFICOL(5)    !method for scaling
    ELSE
     MP(IPLOT)%XCOL  =1    !column used for X-COORDINATE
     MP(IPLOT)%YCOL  =2    !column used for Y-COORDINATE
     MP(IPLOT)%HCOL  =0    !no column used for highlighting
     MP(IPLOT)%HCOL_METHOD=1    !method for scaling
    ENDIF
    MP(IPLOT)%ZCOL   =1    !column used for Z-COORDINATE, default x-coordinate
    MP(IPLOT)%IAXES  =1    !all columns to be plotted on the first axes
    MP(IPLOT)%FADEOUT=1    !fadeout
    MP(IPLOT)%SYMBOL =14   !symbol
    MP(IPLOT)%THICKNESS=1  !thickness
    MP(IPLOT)%TSIZE    =7  !textsize
    MP(IPLOT)%ASSCOL1=2 !## borehole plotting
    MP(IPLOT)%ASSCOL2=0 !## borehole plotting
    MP(IPLOT)%PCOL   =0    !column for plotting

   !## iff
   CASE (3)

    MP(IPLOT)%PRFTYPE=1    !active/non active in profile
    MP(IPLOT)%IDFI   =250  !sight (m)
    MP(IPLOT)%SCOLOR =WRGB(100,100,100)! single - colour    !no colouring, attribute colouring
    IF(PRESENT(ISTYLE))THEN
     MP(IPLOT)%ILEG   =ISTYLE    !no legend used for plotting, use colour in %scolor
     MP(IPLOT)%IATTRIB=6    !cumtt
    ELSE
     MP(IPLOT)%ILEG   =0    !no legend used for plotting, use colour in %scolor
     MP(IPLOT)%IATTRIB=1    !initial label
    ENDIF
    MP(IPLOT)%FADEOUT=0    !fadeout
    MP(IPLOT)%SYMBOL =0    !symbol
    MP(IPLOT)%THICKNESS=1  !thickness

   CASE (4) !## isg 
    MP(IPLOT)%SYMBOL =0    !symbol
    MP(IPLOT)%THICKNESS=1  !thickness

   !## mdf
   CASE (5)
  
    IF(READMDF(IDFNAME,N))THEN
     MP(IPLOT)%LEG%NCLR  =MDF(MP(IPLOT)%NLIDF)%LEG%NCLR
     MP(IPLOT)%LEG%CLASS =MDF(MP(IPLOT)%NLIDF)%LEG%CLASS
     MP(IPLOT)%LEG%RGB   =MDF(MP(IPLOT)%NLIDF)%LEG%RGB
     MP(IPLOT)%LEG%LEGTXT=MDF(MP(IPLOT)%NLIDF)%LEG%LEGTXT
     CALL MDFDEALLOCATE()
     MP(IPLOT)%THICKNESS=1  !contour width
     CALL UTL_READARRAY((/1,0,0/),3,MP(IPLOT)%IDFKIND)
    ENDIF

   !## gen
   CASE (6)

    MP(IPLOT)%IEQ    =0    !no value plotted <--- used as binaire pointer for label plotting
    MP(IPLOT)%ILEG   =0    !no legend used for plotting, use colour in %scolor
    MP(IPLOT)%IATTRIB=1    !initial first label for colouring
    MP(IPLOT)%SCOLOR =WRGB(100,100,100)! single - colour    !no colouring, attribute colouring
    MP(IPLOT)%TSIZE  =7    !textsize
    MP(IPLOT)%PRFTYPE=0    !filled in (0=no,1=yes)
    MP(IPLOT)%THICKNESS=1  !line thickness

  END SELECT

  MP(IPLOT)%ISCREEN=1  !usage of screen (profile)

  !## close idf/ivf/ipf/iff/isg/gen
  DO I=1,MAXFILES; IF(IU(I).GT.0)CLOSE(IU(I)); END DO

  !## try to read in legfile
  IF(.NOT.LLEG)THEN
   CALL LEG_READ(MP(IPLOT)%LEG,LEGNAME,IOS)
   !## error occured, generate a internal legend
   IF(IOS.NE.0)LLEG=.TRUE.
  ENDIF

  !## generate linear legend for entire domain and write it if not assigned to file at this stage
  IF(LLEG)THEN
   SELECT CASE (MP(IPLOT)%IPLOT)
    !## 1=idf,2=ipf,3=iff,5=mdf,6=gen,7=udf
    CASE (1:3,5,6)
     IF(.NOT.LEG_CREATE_CLASSES('LIN','ALE',IPLOT))THEN; MP(IPLOT)%IPLOT=0; EXIT; ENDIF
    !## isg
    CASE (4)
     DR=1.0D0/REAL(MXCLR+1); MP(IPLOT)%LEG%CLASS(0)=1.0D0
     DO I=1,MXCLR; MP(IPLOT)%LEG%CLASS(I)=MP(IPLOT)%LEG%CLASS(I-1)-DR; END DO
     MP(IPLOT)%LEG%NCLR=MXCLR
   END SELECT
   MP(IPLOT)%LEG%LEGTXT    =''      !## default name of the legend file
   MP(IPLOT)%LEG%CGRAD     =1       !## all checkboxes selected
   !## apply default colours
   DO I=1,MXCGRAD; MP(IPLOT)%LEG%ICLRGRAD(I)=WRGB(CLR(I,1),CLR(I,2),CLR(I,3)); ENDDO
   !## create all colours based upon iclrgrad
   CALL LEG_CREATE_COLORS(IPLOT)
  ENDIF

  !## increase number of active plots
  IF(MPW%NACT.GE.MXMPLOT)EXIT
  !## terminate in case GEF is read in
  IF(LGEF)EXIT
 
 ENDDO

 DEALLOCATE(FNAMES)

 CALL UTL_MESSAGEHANDLE(1)

 !## fill manager
 CALL MANAGER_UTL_FILL()

 !## update manager
 CALL MANAGER_UTL_UPDATE()

 END SUBROUTINE MANAGER_UTL_ADDFILE

 !###======================================================================
 LOGICAL FUNCTION MANAGER_UTL_GROUP(FNAME)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(INOUT) :: FNAME
 INTEGER :: IPLOT,N
 
 MANAGER_UTL_GROUP=.FALSE.
 
 N=0
 DO IPLOT=1,MXMPLOT
  IF(MP(IPLOT)%ISEL.AND.MP(IPLOT)%IPLOT.EQ.1)N=N+1
 ENDDO
 
 IF(N.EQ.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Select more than one IDF file in the iMOD Manager to group.','Error')
  RETURN
 ENDIF
 
 IF(TRIM(FNAME).EQ.'')THEN
  IF(.NOT.UTL_WSELECTFILE('iMOD Multi Data File (*.mdf)|*.mdf|',         &
                   SAVEDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,FNAME, &
                   'Save Multi Data File (*.mdf)'))RETURN
 ENDIF
 
 CALL MDFDEALLOCATE(); CALL MDFALLOCATE()

 N=0
 DO IPLOT=1,MXMPLOT
  IF(MP(IPLOT)%ISEL.AND.MP(IPLOT)%IPLOT.EQ.1)THEN
   N=N+1
   MDF(N)%FNAME     =MP(IPLOT)%IDFNAME
   MDF(N)%ALIAS     =MP(IPLOT)%ALIAS
   MDF(N)%SCOLOR    =MP(IPLOT)%SCOLOR
   MDF(N)%PRFTYPE   =MP(IPLOT)%PRFTYPE
   MDF(N)%LEG%NCLR  =MP(IPLOT)%LEG%NCLR
   MDF(N)%LEG%CGRAD =MP(IPLOT)%LEG%CGRAD
   MDF(N)%LEG%CLASS =MP(IPLOT)%LEG%CLASS
   MDF(N)%LEG%LEGTXT=MP(IPLOT)%LEG%LEGTXT
   MDF(N)%LEG%RGB   =MP(IPLOT)%LEG%RGB
  ENDIF
 ENDDO

 IF(.NOT.WRITEMDF(FNAME,N))THEN
  CALL MDFDEALLOCATE()
  RETURN
 ENDIF
 
 !## delete all idf from manager
 CALL MANAGER_UTL_DELETE(IQ=0)

 CALL MDFDEALLOCATE()
 
 MANAGER_UTL_GROUP=.TRUE.
 
 END FUNCTION MANAGER_UTL_GROUP

 !###======================================================================
 SUBROUTINE MANAGER_UTL_UNGROUP()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IPLOT,I,J,M,N
 CHARACTER(LEN=256),DIMENSION(:),ALLOCATABLE :: CFNAME

 !## store mdf-files
 M=0
 DO IPLOT=1,MXMPLOT
  IF(MP(IPLOT)%ISEL.AND.MP(IPLOT)%IPLOT.EQ.5)THEN
   M=M+1
  ENDIF
 ENDDO
 ALLOCATE(CFNAME(M))
 M=0
 DO IPLOT=1,MXMPLOT
  IF(MP(IPLOT)%ISEL.AND.MP(IPLOT)%IPLOT.EQ.5)THEN
   M        =M+1
   CFNAME(M)=MP(IPLOT)%IDFNAME
  ENDIF
 ENDDO

 !## number of mdf files to be ungrouped
 DO I=1,M
  DO IPLOT=1,MXMPLOT
   !## found current mdf to be ungrouped
   IF(MP(IPLOT)%IPLOT.EQ.5.AND.MP(IPLOT)%IDFNAME.EQ.CFNAME(I))EXIT
  ENDDO
  IF(IPLOT.LE.MXMPLOT)THEN
   MP%ISEL       =.FALSE.
   MP(IPLOT)%ISEL=.TRUE.
   !## delete mdf from manager
   CALL MANAGER_UTL_DELETE(IQ=0)

   IF(READMDF(CFNAME(I),N))THEN
    DO J=1,N
     !## read *.mdf file, only to get selected idf to be plotted
     CALL MANAGER_UTL_ADDFILE(MDF(J)%FNAME)
     DO IPLOT=1,MXMPLOT
      IF(MP(IPLOT)%ISEL)THEN
       MP(IPLOT)%ALIAS     =MDF(J)%ALIAS
       MP(IPLOT)%SCOLOR    =MDF(J)%SCOLOR
       MP(IPLOT)%PRFTYPE   =MDF(J)%PRFTYPE
       MP(IPLOT)%LEG%NCLR  =MDF(J)%LEG%NCLR
       MP(IPLOT)%LEG%CGRAD =MDF(J)%LEG%CGRAD
       MP(IPLOT)%LEG%CLASS =MDF(J)%LEG%CLASS
       MP(IPLOT)%LEG%LEGTXT=MDF(J)%LEG%LEGTXT
       MP(IPLOT)%LEG%RGB   =MDF(J)%LEG%RGB
      ENDIF
     END DO
    ENDDO
    CALL MDFDEALLOCATE()
   ENDIF
  ENDIF
 ENDDO

 DEALLOCATE(CFNAME)

 END SUBROUTINE MANAGER_UTL_UNGROUP

 !###======================================================================
 SUBROUTINE MANAGER_UTL_INIT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 CALL WDIALOGLOAD(ID_DMANAGER)

 CALL WDIALOGSELECT(ID_DMANAGERTAB1)
 
 CALL WDIALOGPUTIMAGE(ID_OPEN,ID_ICONOPENIDF,1)
 CALL WDIALOGTOOLTIP(ID_OPEN,'Open an IDF/IFF/IPF/GEN file')
 
 CALL WDIALOGPUTIMAGE(ID_INFO,ID_ICONINFO,1)
 CALL WDIALOGTOOLTIP(ID_INFO,'Display Properties of the Selected File(s)')
 
 CALL WDIALOGPUTIMAGE(ID_DELETE,ID_ICONDELETE,1)
 CALL WDIALOGTOOLTIP(ID_DELETE,'Remove Selected File(s) from the iMOD Manager')
 
 CALL WDIALOGPUTIMAGE(ID_DRAW,ID_ICONREDRAW,1)
 CALL WDIALOGTOOLTIP(ID_DRAW,'Redraw the Selected File(s)')

 CALL WDIALOGPUTIMAGE(ID_IDFVALUE,ID_ICONSELECTPOINT,1)
 CALL WDIALOGTOOLTIP(ID_IDFVALUE,'Start the IDF Value Inspector')

 CALL WDIALOGPUTIMAGE(ID_LEGEND,ID_ICONLEGEND,1)
 CALL WDIALOGTOOLTIP(ID_LEGEND,'Start the Create Legend Window')

 CALL WDIALOGPUTIMAGE(ID_MOVEUP,ID_ICONMOVEUP,1)
 CALL WDIALOGTOOLTIP(ID_MOVEUP,'Shift all Selected Files in a Single Position Upwards')

 CALL WDIALOGPUTIMAGE(ID_MOVEDOWN,ID_ICONMOVEDOWN,1)
 CALL WDIALOGTOOLTIP(ID_MOVEDOWN,'Shift all Selected Files in a Single Position Downwards')

 CALL WDIALOGPUTIMAGE(ID_MATH,ID_ICONCALC,1)
 CALL WDIALOGTOOLTIP(ID_MATH,'Start the IDF Calculator')

 CALL WDIALOGPUTIMAGE(ID_PROPERTIES,ID_ICONPROPERTIES,1)
 CALL WDIALOGTOOLTIP(ID_PROPERTIES,'Select the Appearance of Files in the iMOD Manager')

 CALL WDIALOGPUTIMAGE(ID_FIND,ID_ICONFIND,1)
 CALL WDIALOGTOOLTIP(ID_FIND,'Select Files upon their Particular Filesnames')

 CALL WDIALOGSELECT(ID_DMANAGERTAB2)
 CALL WDIALOGPUTIMAGE(ID_OPEN,ID_ICONOPENIDF,1)
 CALL WDIALOGPUTIMAGE(ID_DELETE,ID_ICONDELETE,1)
 CALL WDIALOGPUTIMAGE(ID_DRAW,ID_ICONREDRAW,1)
 CALL WDIALOGPUTIMAGE(ID_MOVEUP,ID_ICONMOVEUP,1)
 CALL WDIALOGPUTIMAGE(ID_MOVEDOWN,ID_ICONMOVEDOWN,1)
 CALL WDIALOGPUTIMAGE(ID_LEGEND,ID_ICONLEGEND,1)

 CALL WDIALOGSELECT(ID_DMANAGERTAB3)
 CALL WDIALOGPUTIMAGE(ID_NEW,ID_ICONNEW,1)
 CALL WDIALOGPUTIMAGE(ID_OPEN,ID_ICONINFO,1)
 CALL WDIALOGPUTIMAGE(ID_DELETE,ID_ICONDELETE,1)
 CALL WDIALOGPUTIMAGE(ID_DRAW,ID_ICONREDRAW,1)
 CALL WDIALOGPUTIMAGE(ID_TAGOWNER,ID_ICONTAGOWNER,1)

 CALL WDIALOGSELECT(ID_DMANAGERTAB4)
 CALL WDIALOGPUTIMAGE(ID_LEGEND,ID_ICONLEGEND,1)

 !## initialize legend-memory if mp object
 DO I=1,MXMPLOT; CALL LEG_ALLOCATE(MP(I)%LEG); END DO

 CALL WDIALOGLOAD(ID_DMANAGERPROPERTIES,ID_DMANAGERPROPERTIES)

 END SUBROUTINE MANAGER_UTL_INIT

 !###======================================================================
 SUBROUTINE MANAGER_UTL_SHOW()
 !###======================================================================
 IMPLICIT NONE

 CALL WINDOWSELECT(0)
 IF(WMENUGETSTATE(ID_MANAGER,2).EQ.1)THEN
  CALL MANAGER_UTL_CLOSE()
  RETURN
 ENDIF
 CALL WMENUSETSTATE(ID_MANAGER,2,1)

 CALL WDIALOGSELECT(ID_DMANAGER)
 CALL WDIALOGSHOW(0,65,0,2)
 CALL MANAGER_UTL_UPDATE()

 END SUBROUTINE MANAGER_UTL_SHOW

 !###======================================================================
 SUBROUTINE MANAGER_UTL_CLOSE()
 !###======================================================================
 IMPLICIT NONE

 CALL WINDOWSELECT(0); CALL WMENUSETSTATE(ID_MANAGER,2,0)

 CALL WDIALOGSELECT(ID_DMANAGER); CALL WDIALOGHIDE()

 CALL WINDOWSELECT(MPW%IWIN)

 END SUBROUTINE MANAGER_UTL_CLOSE

 !###======================================================================
 SUBROUTINE MANAGER_UTL_DELETE(IQ)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN),OPTIONAL :: IQ
 INTEGER :: IPLOT,JPLOT
 INTEGER :: I

 I=1; IF(PRESENT(IQ))I=IQ
 IF(I.EQ.1)THEN
  CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Are you sure to remove the selected files from the iMOD manager?','Question')
  IF(WINFODIALOG(4).NE.1)RETURN
 ENDIF

 !## delete all selections
 IPLOT=1
 DO WHILE(IPLOT.LE.MXMPLOT)
  !## remove plot
  IF(MP(IPLOT)%ISEL)THEN
   DO JPLOT=IPLOT,MXMPLOT-1
    MP(JPLOT)     =MP(JPLOT+1)
    DRWLIST(JPLOT)=DRWLIST(JPLOT+1)
    ACTLIST(JPLOT)=ACTLIST(JPLOT+1)
    !## deallocate idf if existing after removal
    CALL IDFDEALLOCATEX(MP(JPLOT)%IDF)
   END DO
   DRWLIST(MXMPLOT)=0
   ACTLIST(MXMPLOT)=0
   MP(MXMPLOT)%ISEL=.FALSE.
   MP(MXMPLOT)%IACT=.FALSE.
   !## deallocate idf if existing after removal
   CALL IDFDEALLOCATEX(MP(MXMPLOT)%IDF)
  ELSE
   IPLOT=IPLOT+1
  ENDIF
 END DO

 CALL MANAGER_UTL_FILL()
 CALL MANAGER_UTL_UPDATE()

 END SUBROUTINE MANAGER_UTL_DELETE
 
 !###======================================================================
 SUBROUTINE MANAGER_UTL_FILL()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,IPLOT,IOPT
 CHARACTER(LEN=256),DIMENSION(MXMPLOT) :: ACTIDF   !## which idf is active in manager

 CALL WDIALOGSELECT(ID_DMANAGERPROPERTIES)
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,IOPT)

 ACTIDF  =''
 ACTLIST =0
 MPW%NACT=0
 DO IPLOT=1,MXMPLOT
  IF(MP(IPLOT)%IACT)THEN
   MPW%NACT=MPW%NACT+1
   I=INDEXNOCASE(MP(IPLOT)%IDFNAME,'\',.TRUE.)+1
   SELECT CASE (IOPT)
    CASE (1)
     ACTIDF(MPW%NACT)=MP(IPLOT)%IDFNAME(I:)
    CASE (2)
     ACTIDF(MPW%NACT)=TRIM(MP(IPLOT)%IDFNAME(I:))//' ( '//MP(IPLOT)%IDFNAME(:I-2)//' )'
    CASE (3,4)
     ACTIDF(MPW%NACT)=TRIM(MP(IPLOT)%IDFNAME)
    CASE (5)
     ACTIDF(MPW%NACT)=TRIM(MP(IPLOT)%ALIAS)
   END SELECT
   IF(MP(IPLOT)%ISEL)ACTLIST(MPW%NACT)=1
  ENDIF
 ENDDO

 IF(IOPT.EQ.4)CALL UTL_GETRELEVANTDIR(ACTIDF,MPW%NACT)

 CALL WDIALOGSELECT(ID_DMANAGERTAB1)
 IF(MPW%NACT.GT.0)THEN
  IF(WINFODIALOGFIELD(ID_DMTABMENU,FIELDSTATE).NE.1)CALL WDIALOGFIELDSTATE(ID_DMTABMENU,1)
  CALL WDIALOGPUTMENU(ID_DMTABMENU,ACTIDF,MPW%NACT,ACTLIST)
 ELSE
  CALL WDIALOGCLEARFIELD(ID_DMTABMENU)
  IF(WINFODIALOGFIELD(ID_DMTABMENU,FIELDSTATE).NE.2)CALL WDIALOGFIELDSTATE(ID_DMTABMENU,2)
 ENDIF

 END SUBROUTINE MANAGER_UTL_FILL

 !###======================================================================
 SUBROUTINE MANAGER_UTL_UPDATE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IPLOT,I,J,MIPF,MIDF,MIFF,MISG,MMDF,MGEN,MUDF

 CALL WDIALOGSELECT(ID_DMANAGERTAB1)

 MP%ISEL=.FALSE.
 ACTLIST=0
 IF(MPW%NACT.GT.0)THEN
  CALL WDIALOGGETMENU(ID_DMTABMENU,ACTLIST)
  DO IPLOT=1,MXMPLOT; IF(ACTLIST(IPLOT).EQ.1)MP(IPLOT)%ISEL=.TRUE.; END DO
 ENDIF

 MIPF=0  !IPF SELECTED
 MIDF=0  !IDF SELECTED
 MIFF=0  !IFF SELECTED
 MISG=0  !ISG SELECTED
 MMDF=0  !MDF SELECTED
 MGEN=0  !GEN SELECTED
 MUDF=0  !UDF SELECTED
 DO IPLOT=1,MXMPLOT
  IF(MP(IPLOT)%ISEL)THEN
   IF(MP(IPLOT)%IPLOT.EQ.1)MIDF=MIDF+1
   IF(MP(IPLOT)%IPLOT.EQ.2)MIPF=MIPF+1
   IF(MP(IPLOT)%IPLOT.EQ.3)MIFF=MIFF+1
   IF(MP(IPLOT)%IPLOT.EQ.4)MISG=MISG+1
   IF(MP(IPLOT)%IPLOT.EQ.5)MMDF=MMDF+1
   IF(MP(IPLOT)%IPLOT.EQ.6)MGEN=MGEN+1
   IF(MP(IPLOT)%IPLOT.EQ.7)MUDF=MUDF+1
  ENDIF
 ENDDO

 CALL WINDOWSELECT(0)

 !## zoomprevious and zoomnext settings
 I=0; IF(ZM%IZOOM.GT.1)       I=1; CALL WMENUSETSTATE(ID_ZOOMPREVIOUS,1,I)
 I=0; IF(ZM%IZOOM.LT.ZM%NZOOM)I=1; CALL WMENUSETSTATE(ID_ZOOMNEXT,1,I)

 !## always off
 CALL WMENUSETSTATE(ID_SFIGURE,1,0)

 !## nothing active
 I=1; IF(MPW%NACT.EQ.0)I=0

 IF(WMENUGETSTATE(ID_ACCURACY,1).NE.I)        CALL WMENUSETSTATE(ID_ACCURACY,1,I)
 IF(WINFODIALOGFIELD(ID_DRAW,FIELDSTATE).NE.I)CALL WDIALOGFIELDSTATE(ID_DRAW,I)
 IF(WMENUGETSTATE(ID_IMODINFO,1).NE.I)        CALL WMENUSETSTATE(ID_IMODINFO,1,I)
 IF(WINFODIALOGFIELD(ID_INFO,FIELDSTATE).NE.I)CALL WDIALOGFIELDSTATE(ID_INFO,I)

 !## nothing selected
 I=1; IF(SUM(ACTLIST).EQ.0)I=0
 IF(WINFODIALOGFIELD(ID_DELETE,FIELDSTATE).NE.I)CALL WDIALOGFIELDSTATE(ID_DELETE,I)

 I=0; IF(MIDF+MUDF.GT.1)I=1; CALL WMENUSETSTATE(ID_SORT,1,I)

 !## at least one selected
 I=0; IF(SUM(ACTLIST).GT.0)I=1
 !## not sustained whenever nod idf,ipf,iff,mdf is selected
 IF(MIDF+MIPF+MIFF+MMDF+MUDF.EQ.0)I=0
 IF(WMENUGETSTATE(ID_PROFILE,1).NE.I)     CALL WMENUSETSTATE(ID_PROFILE,1,I)
 IF(WMENUGETSTATE(ID_TIMESERIES,1).NE.I)  CALL WMENUSETSTATE(ID_TIMESERIES,1,I)
 IF(WMENUGETSTATE(ID_MOVIE_CREATE,1).NE.I)CALL WMENUSETSTATE(ID_MOVIE_CREATE,1,I)
 !## not sustained to have more than one mdf in the 3d-tool
 IF(MMDF.GT.1)I=0
 IF(WMENUGETSTATE(ID_3DTOOL,1).NE.I)CALL WMENUSETSTATE(ID_3DTOOL,1,I)

 !## legend status
 I=0; IF(SUM(ACTLIST).GT.0)I=1
 J=0
 IF(MIDF.GT.0)J=J+1
 IF(MIPF.GT.0)J=J+1
 IF(MISG.GT.0)J=J+1
 IF(MIFF.GT.0)J=J+1
 IF(MMDF.GT.0)J=J+1
 IF(MGEN.GT.0)J=J+1
 IF(MUDF.GT.0)J=J+1
 !## not one single extension selected
 IF(J.NE.1)I=0

 IF(MMDF.GT.1)I=0 !## no legend for mdf files
 !## only if %ileg=1 in case iff,ipf and gen files
 IF(MGEN+MIPF+MIFF+MUDF.GT.0)THEN
  DO IPLOT=1,MXMPLOT; IF(ACTLIST(IPLOT).EQ.1)EXIT; END DO
  IF(MP(IPLOT)%ILEG.EQ.0)I=0
 ENDIF
 IF(WMENUGETSTATE(ID_LEGCD,1).NE.I)CALL WMENUSETSTATE(ID_LEGCD,1,I)

 !## not for ipf,iff,gen's
 J=I; IF(MIPF+MIFF+MGEN.GT.0)J=0
 IF(WMENUGETSTATE(ID_LEGED,1).NE.J)CALL WMENUSETSTATE(ID_LEGED,1,J)
 IF(WMENUGETSTATE(ID_TDLL,1).NE.J) CALL WMENUSETSTATE(ID_TDLL,1,J) !## total domain
 IF(WMENUGETSTATE(ID_CDLL,1).NE.I) CALL WMENUSETSTATE(ID_CDLL,1,I) !## current domain

 IF(MIDF+MIPF+MISG+MIFF+MMDF+MGEN+MUDF.NE.1)I=0
 IF(WMENUGETSTATE(ID_ADJUSTLEGEND,1).NE.I)CALL WMENUSETSTATE(ID_ADJUSTLEGEND,1,I)
 IF(WINFODIALOGFIELD(ID_LEGEND,FIELDSTATE).NE.I)CALL WDIALOGFIELDSTATE(ID_LEGEND,I)

 I=1; IF(MIDF+MMDF+MUDF.EQ.0)I=0 !## only for idf and mdf
 IF(WMENUGETSTATE(ID_CDUV,1).NE.I) CALL WMENUSETSTATE(ID_CDUV,1,I)  !## unique legend CURRENT domain
 IF(WMENUGETSTATE(ID_TDUV,1).NE.I) CALL WMENUSETSTATE(ID_TDUV,1,I)  !## unique legend TOTAL domain
 IF(WMENUGETSTATE(ID_CDLNL,1).NE.I)CALL WMENUSETSTATE(ID_CDLNL,1,I) !## percentile legend CURRENT domain
 IF(WMENUGETSTATE(ID_TDLNL,1).NE.I)CALL WMENUSETSTATE(ID_TDLNL,1,I) !## percentile legend TOTAL domain

 !## more than one selected
 I=0; IF(SUM(ACTLIST).GT.1)I=1
 IF(WMENUGETSTATE(ID_SYNLEGENDS,1).NE.I)CALL WMENUSETSTATE(ID_SYNLEGENDS,1,I)

 !## movement of idf in datamanager
 I=0
 J=0
 IF(SUM(ACTLIST).GT.0)THEN
  IF(MPW%NACT.GT.1.AND.MPW%NACT.LT.MXMPLOT)THEN
   IF(.NOT.MP(1)%ISEL)       I=1
   IF(.NOT.MP(MPW%NACT)%ISEL)J=1
  ENDIF
 ENDIF
 IF(WINFODIALOGFIELD(ID_MOVEUP,FIELDSTATE).NE.I)CALL WDIALOGFIELDSTATE(ID_MOVEUP,I)
 IF(WINFODIALOGFIELD(ID_MOVEDOWN,FIELDSTATE).NE.J)CALL WDIALOGFIELDSTATE(ID_MOVEDOWN,J)

 !## update topography
 CALL TOPO1UPDATEMANAGER() 

 I=1
 IF(MPW%NACT.GE.MXMPLOT)I=0
 IF(WMENUGETSTATE(ID_OPENIDF,1).NE.I)CALL WMENUSETSTATE(ID_OPENIDF,1,I)

 CALL WDIALOGSELECT(ID_DMANAGERTAB1)
 CALL WDIALOGFIELDSTATE(ID_OPEN,I)

 !## settings regarding IDF's
 I=0
 IF(MIDF.GT.0.OR.MMDF.GT.0)I=1
 IF(WMENUGETSTATE(ID_IDFMAPVALUES,1).NE.I)        CALL WMENUSETSTATE(ID_IDFMAPVALUES,1,I)
 IF(WMENUGETSTATE(ID_IDFOPTIONS,1).NE.I)          CALL WMENUSETSTATE(ID_IDFOPTIONS,1,I)
 IF(WMENUGETSTATE(ID_IDFEDIT,1).NE.I)             CALL WMENUSETSTATE(ID_IDFEDIT,1,I)
 IF(WMENUGETSTATE(ID_MAPEXPORT,1).NE.I)           CALL WMENUSETSTATE(ID_MAPEXPORT,1,I)
 IF(WINFODIALOGFIELD(ID_IDFVALUE,FIELDSTATE).NE.I)CALL WDIALOGFIELDSTATE(ID_IDFVALUE,I)

 IF(WMENUGETSTATE(ID_NETCDFFORMAT,1).NE.INETCDF)CALL WMENUSETSTATE(ID_NETCDFFORMAT,1,INETCDF)
 
 I=0
 IF(MIDF.GT.1.AND.MIPF+MISG+MIFF+MMDF+MUDF.EQ.0)I=1
 IF(WMENUGETSTATE(ID_IDFGROUP,1).NE.I)CALL WMENUSETSTATE(ID_IDFGROUP,1,I)
 I=0
 IF(MMDF.GT.0.AND.MIPF+MISG+MIFF+MIDF+MUDF.EQ.0)I=1
 IF(WMENUGETSTATE(ID_IDFUNGROUP,1).NE.I)CALL WMENUSETSTATE(ID_IDFUNGROUP,1,I)

 !## settings regarding IPF's
 I=0
 IF(MIPF.GT.0.AND.MIFF+MISG.EQ.0)I=1
 IF(WMENUGETSTATE(ID_IPFOPTIONS,1).NE.I)CALL WMENUSETSTATE(ID_IPFOPTIONS,1,I)
 IF(WMENUGETSTATE(ID_ANALYSEIPF,1).NE.I) CALL WMENUSETSTATE(ID_ANALYSEIPF,1,I)
 I=0
 IF(MIPF.EQ.1)I=1
 IF(WMENUGETSTATE(ID_EXTRACTIPF,1).NE.I) CALL WMENUSETSTATE(ID_EXTRACTIPF,1,I)
 I=0
 IF(MIPF.EQ.1.AND.MIDF+MIFF+MISG+MMDF+MUDF.EQ.0)I=1
 IF(WMENUGETSTATE(ID_IPFCONFIGURE,1).NE.I) CALL WMENUSETSTATE(ID_IPFCONFIGURE,1,I)

 !## settings regarding IFF's
 I=0
 IF(MIFF.GT.0.AND.MIPF+MISG.EQ.0)I=1
 IF(WMENUGETSTATE(ID_IFFOPTIONS,1).NE.I)CALL WMENUSETSTATE(ID_IFFOPTIONS,1,I)
 I=0
 IF(MIFF.EQ.1.AND.MIDF+MIPF+MISG+MMDF+MUDF.EQ.0)I=1
 IF(WMENUGETSTATE(ID_IFFCONFIGURE,1).NE.1)CALL WMENUSETSTATE(ID_IFFCONFIGURE,1,I)
 
 !## settings regarding ISG's
 I=0
 IF(MISG.GT.0.AND.MIFF+MIPF.EQ.0)I=1
 IF(WMENUGETSTATE(ID_ISGOPTIONS,1).NE.I)CALL WMENUSETSTATE(ID_ISGOPTIONS,1,I)
 IF(WMENUGETSTATE(ID_ISGSHOW,1).NE.I)CALL WMENUSETSTATE(ID_ISGSHOW,1,I)
 IF(WMENUGETSTATE(ID_ISGEDIT,1).NE.1)CALL WMENUSETSTATE(ID_ISGEDIT,1,I)

 !## settings regarding GEN's
 I=0
 IF(MGEN.GT.0)I=1
 IF(WMENUGETSTATE(ID_GENOPTIONS,1).NE.I)CALL WMENUSETSTATE(ID_GENOPTIONS,1,I)
 I=0
 IF(MGEN.EQ.1)I=1
 IF(WMENUGETSTATE(ID_GENCONFIGURE,1).NE.I)CALL WMENUSETSTATE(ID_GENCONFIGURE,1,I)
 IF(WMENUGETSTATE(ID_GENEXPORT,1).NE.I)CALL WMENUSETSTATE(ID_GENEXPORT,1,I)

 I=1
 !## no legend available whenever more than one file or no map is selected
 IF(SUM(ACTLIST).NE.1)I=0
 !## no legend available whenever isg is active
 IF(MISG.GT.0)I=0
 CALL WDIALOGSELECT(ID_DMANAGER)
 !## grey out tab legend
 CALL WDIALOGTABSTATE(ID_DMTAB,ID_DMANAGERTAB4,I)
 !## no comments whenever more than one file is selected AND no tags keyword (TAGS) available
 IF(PREFVAL(7).EQ.'')I=0
 CALL WDIALOGTABSTATE(ID_DMTAB,ID_DMANAGERTAB3,I)

 END SUBROUTINE MANAGER_UTL_UPDATE
 
 !###======================================================================
 SUBROUTINE MANAGER_UTL_MENUFIELDS(ID,K,J)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,PARAMETER :: MAXID=23
 INTEGER,INTENT(IN) :: ID,J,K
 INTEGER :: I
 INTEGER,DIMENSION(MAXID) :: ID1
 DATA (ID1(I),I=1,MAXID) /ID_NEW,ID_OPEN,ID_SAVE,ID_SAVEAS,ID_COPY,         &
                       ID_MANAGER,ID_OPENIDF,ID_ZOOMINMAP,ID_ZOOMOUTMAP, &
                       ID_ZOOMRECTANGLEMAP,ID_ZOOMFULLMAP,ID_MOVEMAP,    &
                       ID_IRDATABASE,ID_IMODINFO,ID_DISTANCE,ID_PROFILE, &
                       ID_TIMESERIES,ID_3DTOOL,ID_TOPOGRAPHY, &
                       ID_ZOOMPREVIOUS,ID_ZOOMNEXT,ID_REDRAW,ID_MOVIE/

 CALL WINDOWSELECT(0)

 !## (de)activate buttons
 DO I=1,MAXID
  IF(ID1(I).NE.ID)CALL WMENUSETSTATE(ID1(I),1,J)
  IF(ID1(I).EQ.ID)CALL WMENUSETSTATE(ID1(I),2,K)
 END DO
 IF(J.EQ.1)THEN
  CALL MANAGER_UTL_UPDATE()
  CALL TOPO1UPDATEMANAGER()
 ENDIF

 END SUBROUTINE MANAGER_UTL_MENUFIELDS
 
END MODULE MOD_MANAGER_UTL