!!  Copyright (C) Stichting Deltares, 2005-2017.
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
!###======================================================================
SUBROUTINE IDFINIT(IDFNAMEGIVEN,LEGNAME,LPLOT,ISTYLE,LDEACTIVATE,IPFICOL,ILABELS,IPFASSFILES) 
!###======================================================================
USE WINTERACTER
USE RESOURCE
USE IMODVAR
USE MODPLOT
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_COLOURS
USE MOD_IDF, ONLY : IDFREAD
USE MOD_ISG_PAR, ONLY : MAXFILES
USE MOD_IPF, ONLY : UTL_GETUNITIPF
USE MOD_MDF, ONLY : READMDF,MDFDEALLOCATE,MDF
USE MOD_UTL, ONLY : UTL_GETUNIT,UTL_MESSAGEHANDLE,UTL_READARRAY,UTL_WSELECTFILE,ITOS,UTL_CAP
USE MOD_ASC2IDF, ONLY : ASC2IDF_IMPORTASC,ASC2IDF_IMPORTASC_TYPE5
USE MOD_NC2IDF, ONLY : NC2IDF_IMPORTNC,INETCDF
USE MOD_LEGEND, ONLY : LEG_CREATE_CLASSES,LEG_CREATE_COLORS
USE MOD_LEGEND_UTL, ONLY : LEG_READ
USE MOD_MANAGER, ONLY : MANAGERFILL,MANAGERUPDATE
USE MOD_IFF, ONLY : UTL_GETUNITIFF
USE MOD_OSD, ONLY : OSD_OPEN
USE MOD_ISG_UTL, ONLY : UTL_GETUNITSISG
USE MOD_MAP2IDF, ONLY : MAP2IDF_IMPORTMAP
USE MOD_GEF2IPF, ONLY : GEF2IPF_MAIN
USE MOD_GEF2IPF_PAR, ONLY : GEFNAMES,IPFFNAME
USE MOD_GENPLOT, ONLY : TOPOSHPTOGEN
USE MOD_UDF_UTL, ONLY : UDF_DEALLOCATEMESH,UDF_OPEN
IMPLICIT NONE
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: IDFNAMEGIVEN
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: LEGNAME
LOGICAL,INTENT(IN),OPTIONAL :: LPLOT,LDEACTIVATE
INTEGER,INTENT(IN),OPTIONAL :: ISTYLE
INTEGER,INTENT(IN),OPTIONAL,DIMENSION(:) :: IPFASSFILES
INTEGER,INTENT(IN),DIMENSION(:),OPTIONAL :: ILABELS
INTEGER,DIMENSION(5),INTENT(IN),OPTIONAL :: IPFICOL
INTEGER :: IDF,NIDF,I,J,K,IOS,IPLOT,N,IACT,M
INTEGER,ALLOCATABLE,DIMENSION(:) :: ILIST
INTEGER,DIMENSION(MAXFILES) :: IU
REAL :: DR
CHARACTER(LEN=10000) :: IDFNAME,IDFLIST
CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: FNAMES
LOGICAL :: LLEG,LPLOTTING,LGEF,LIPF

!## how many active before opening files
IACT=MPW%NACT

!## initialize iu
IU=0

LPLOTTING=.TRUE.
IF(PRESENT(LPLOT))LPLOTTING=LPLOT

IF(.NOT.PRESENT(IDFNAMEGIVEN))THEN
 IDFNAME=''
 IF(INETCDF.EQ.0)THEN
  IF(.NOT.UTL_WSELECTFILE('All Known Files (*.idf;*.udf;*.mdf;*.ipf;*.isg;*.iff;*.asc;*.shp;*.gen;*.gef;*.map)'//&
                   '|*.idf;*.udf;*.mdf;*.ipf;*.isg;*.iff;*.arr;*.asc;*.shp;*.gen;*.gef;*.map|'// &
                   'iMOD Map (*.idf)|*.idf|'               //&
                   'iMOD Unstructered Data File (*.udf)|*.udf|'   //&
                   'iMOD Multi Data File (*.mdf)|*.mdf|'   //&
                   'iMOD Pointers (*.ipf)|*.ipf|'          //&
                   'iMOD Segment-River File (*.isg)|*.isg|'//&
                   'iMOD Flowline File (*.iff)|*.iff|'     //&
                   'iMOD Array File (*.arr)|*.arr|'        //&
                   'ESRI Raster file (*.asc)|*.asc|'       //&
                   'ESRI Shape file (*.shp)|*.shp|'        //&
                   'ESRI Ungenerate file (*.gen)|*.gen|'   //&
                   'GEF file (*.gef)|*.gef|'               //&
                   'PC Raster Map file (*.map)|*.map|',      &
                   LOADDIALOG+MUSTEXIST+PROMPTON+DIRCHANGE+MULTIFILE,IDFNAME,&
                   'Load iMOD Map (*.idf,*.mdf,*.ipf,*.isg,*.iff,*.asc,*.shp,*.gen,*.gef,*.map)'))RETURN
 ELSEIF(INETCDF.EQ.1)THEN
  IF(.NOT.UTL_WSELECTFILE('All Known Files (*.idf;*.udf;*.mdf;*.ipf;*.isg;*.iff;*.nc;*.asc;*.shp;*.gen;*.gef;*.map)'//&
                   '|*.idf;*.udf;*.mdf;*.ipf;*.isg;*.iff,*.arr;*.nc;*.asc;*.shp;*.gen;*.gef;*.map|'// &
                   'iMOD Map (*.idf)|*.idf|'               //&
                   'iMOD Unstructered Data File (*.udf)|*.udf|'   //&
                   'iMOD Multi Data File (*.mdf)|*.mdf|'   //&
                   'iMOD Pointers (*.ipf)|*.ipf|'          //&
                   'iMOD Segment-River File (*.isg)|*.isg|'//&
                   'iMOD Flowline File (*.iff)|*.iff|'     //&
                   'iMOD Array File (*.arr)|*.arr|'        //&
                   'NetCDF File (*.nc)|*.nc|'              //&
                   'ESRI Raster file (*.asc)|*.asc|'       //&
                   'ESRI Shape file (*.shp)|*.shp|'       //&
                   'ESRI Ungenerate file (*.gen)|*.gen|'   //&
                   'GEF file (*.gef)|*.gef|'               //&
                   'PC Raster Map file (*.map)|*.map|',      &
                   LOADDIALOG+MUSTEXIST+PROMPTON+DIRCHANGE+MULTIFILE,IDFNAME,&
                   'Load iMOD Map (*.idf,*.mdf,*.ipf,*.isg,*.iff,*.nc,*.asc,*.shp,*.gen,*.gef,*.map)'))RETURN
 ENDIF
ELSE
 IDFNAME=IDFNAMEGIVEN
ENDIF

CALL UTL_MESSAGEHANDLE(0)

K=INDEX(IDFNAME,CHAR(0))
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

ALLOCATE(FNAMES(NIDF))
DO IDF=1,NIDF
 !## construct new name in multi-file selection mode
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
 CALL IUPPERCASE(FNAMES(IDF))
ENDDO

LGEF=.FALSE.
DO IDF=1,NIDF

 IDFNAME=FNAMES(IDF)
 
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
    CALL ASC2IDF_IMPORTASC(IDFNAME,0.0,0.0,I,0)
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
  CASE ('SHP')
   !## transform shp/dbf -> gen/dat
   IF(.NOT.TOPOSHPTOGEN(TRIM(IDFNAME),LIPF))CYCLE
   IF(LIPF)THEN
    MP(IPLOT)%IPLOT=2; IDFNAME=IDFNAME(:INDEX(IDFNAME,'.',.TRUE.)-1)//'.IPF'
   ELSE
    MP(IPLOT)%IPLOT=6; IDFNAME=IDFNAME(:INDEX(IDFNAME,'.',.TRUE.)-1)//'.GEN'
   ENDIF
  CASE ('UDF')
   MP(IPLOT)%IPLOT=7
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
  IU(1)=UTL_GETUNITIFF(IDFNAME,'OLD'); IF(IU(1).LE.0)MP(IPLOT)%IPLOT=0
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
 !## open udf-file
 ELSEIF(MP(IPLOT)%IPLOT.EQ.7)THEN
  IF(.NOT.UDF_OPEN(MP(IPLOT)%IDF,IDFNAME,0,IU(1)))THEN;
   CALL UDF_DEALLOCATEMESH(); MP(IPLOT)%IPLOT=0
  ENDIF
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
    IF(MP(IPLOT)%IDF%IVF.EQ.0)THEN
     CALL UTL_READARRAY((/1,0,0/),3,MP(IPLOT)%IDFKIND)
    ELSE
     CALL UTL_READARRAY((/0,0,1/),3,MP(IPLOT)%IDFKIND)
    ENDIF
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
    MP(IPLOT)%IEQ=-1.0*MP(IPLOT)%IEQ    
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
!  MP(IPLOT)%SCOLOR =0    !no colouring, attribute colouring
   IF(PRESENT(ISTYLE))THEN
    MP(IPLOT)%ILEG   =ISTYLE    !no legend used for plotting, use colour in %scolor
    MP(IPLOT)%IATTRIB=6    !cumtt
   ELSE
    MP(IPLOT)%ILEG   =0    !no legend used for plotting, use colour in %scolor
    MP(IPLOT)%IATTRIB=1    !initial label
   ENDIF
!  MP(IPLOT)%IDFKIND=0    !nog vrij te gebruiken
!  MP(IPLOT)%IEQ    =0    !nog vrij te gebruiken
!  MP(IPLOT)%UNITS  =0    !nog vrij te gebruiken
   MP(IPLOT)%FADEOUT=0    !fadeout
   MP(IPLOT)%SYMBOL =0    !symbol
   MP(IPLOT)%THICKNESS=1  !thickness

  CASE (4) !## isg 
!  MP(IPLOT)%PRFTYPE=0    !nog vrij te gebruiken
!  MP(IPLOT)%IDFI   =0    !nog vrij te gebruiken
!  MP(IPLOT)%SCOLOR =0    !nog vrij te gebruiken
!  MP(IPLOT)%IDFKIND=0    !nog vrij te gebruiken
!  MP(IPLOT)%IEQ    =0    !nog vrij te gebruiken
!  MP(IPLOT)%UNITS  =0    !nog vrij te gebruiken
!  MP(IPLOT)%FADEOUT=0    !nog vrij te gebruiken
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
   MP(IPLOT)%TSIZE  =2  !textsize
   MP(IPLOT)%PRFTYPE=0  !filled in (0=no,1=yes)
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
    DR=1.0/REAL(MXCLR+1); MP(IPLOT)%LEG%CLASS(0)=1.0
    DO I=1,MXCLR; MP(IPLOT)%LEG%CLASS(I)=MP(IPLOT)%LEG%CLASS(I-1)-DR; END DO
    MP(IPLOT)%LEG%NCLR=MXCLR
  END SELECT
  MP(IPLOT)%LEG%LEGTXT    =''      !## default name of the legend file
  MP(IPLOT)%LEG%CGRAD     =1       !## all checkboxes selected
  !## apply default colours
  DO I=1,MXCGRAD; MP(IPLOT)%LEG%ICLRGRAD(I)=WRGB(CLR(I,1),CLR(I,2),CLR(I,3)); ENDDO
  !# create all colours based upon iclrgrad
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
CALL MANAGERFILL()

!## plot selected sets
IF(LPLOTTING)THEN
 !## zoom direct to the map(s)
! IF(IACT.EQ.0)CALL IDFZOOM(ID_ZOOMFULLMAP,0.0,0.0,0)
 CALL IDFPLOTFAST(1)
ENDIF

!## update manager
CALL MANAGERUPDATE()

RETURN
END SUBROUTINE

!###======================================================================
SUBROUTINE IDFPLOTFAST(IFAST)
!###======================================================================
USE WINTERACTER
USE RESOURCE
USE MODPLOT
USE IMODVAR
USE MOD_LEGPLOT, ONLY : LEGPLOT_PLOT_SHOW
USE MOD_UTL, ONLY : UTL_GETUNIT,ITOS
USE MOD_3D_SETTINGS, ONLY : IMOD3D_DISPLAY_UPDATE
IMPLICIT NONE
INTEGER,INTENT(IN) :: IFAST
INTEGER :: I,J,N,IPLOT
INTEGER,DIMENSION(4) :: ID
DATA                   (ID(I),I=1,4) /ID_LOWACCURACY, ID_MEDIUMACCURACY, &
                                      ID_HIGHACCURACY,ID_EXCELLENTACCURACY/

!## check whether transparancy need to be checked off
IF(WMENUGETSTATE(ID_TRANSPARANTIDF,2))THEN !.EQ.0.AND.   &
!   WMENUGETSTATE(ID_TRANSPARANTNODATAIDF,2).EQ.0)THEN
 N=0
 DO IPLOT=1,MXMPLOT
  IF(MP(IPLOT)%ISEL.AND.MP(IPLOT)%IPLOT.EQ.1.OR.MP(IPLOT)%IPLOT.EQ.4)N=N+1
 ENDDO
 IF(N.GT.10)THEN
  CALL WMESSAGEBOX(YESNOCANCEL,QUESTIONICON,COMMONYES,'Are you sure to display '//TRIM(ITOS(N))//' IDF files in transparancy?'//CHAR(13)// &
   'If not, choose [No] and iMOD turn transparancy off','Question')
  IF(WINFODIALOG(1).EQ.2)CALL WMENUSETSTATE(ID_TRANSPARANTIDF,2,0)
 ENDIF
ENDIF

IF(IFAST.EQ.0)THEN
 CALL IDFPLOT(1)
ELSE
 CALL WINDOWSELECT(0)
 DO I=1,4
  IF(WMENUGETSTATE(ID(I),2).EQ.1)EXIT
 END DO
 CALL WMENUSETSTATE(ID(I),2,0)

 DO J=1,I,MAX(1,I-1)

  CALL WMENUSETSTATE(ID(J),2,1)
  IF(J.EQ.I)THEN
   CALL IDFPLOT(1)  !## final
  ELSE
   CALL IDFPLOT(0)  !## rough
  ENDIF
  CALL WMENUSETSTATE(ID(J),2,0)
 END DO
 CALL WMENUSETSTATE(ID(I),2,1)
ENDIF

!## refresh legend in legend tab
CALL LEGPLOT_PLOT_SHOW()

CALL IMOD3D_DISPLAY_UPDATE(MPW%XMIN,MPW%YMIN,MPW%XMAX,MPW%YMAX)

RETURN
END SUBROUTINE

!###======================================================================
SUBROUTINE IDFPLOT(IPLOTFAST)
!###======================================================================
USE WINTERACTER
USE RESOURCE
USE MOD_POLYGON_PAR
USE MODPLOT
USE IMODVAR
USE MOD_POLYGON_DRAW, ONLY : POLYGON1DRAWSHAPE,POLYGON1DRAWYSEL
USE MOD_IPF, ONLY : IPFDRAW,IPFINIT
USE MOD_IPF_PAR, ONLY : NIPF
USE MOD_IDF, ONLY : IDFREAD,IDFDEALLOCATEX,IDFGETLOC,IDFGETXYVAL
USE MOD_GENPLOT, ONLY : TOPOGENDRAW,GENDRAW
USE MOD_IR_PAR, ONLY : IRWIN
USE MOD_IR_FIELDS, ONLY : IR1DRAWSHAPES
USE MOD_IR_GEN, ONLY : IR1GENDRAW
USE MOD_UTL, ONLY : UTL_GETUNIT,UTL_MESSAGEHANDLE,UTL_IDFCRDCOR,UTL_FILLARRAY,IDFPLOT1BITMAP,IDFPLOT2BITMAP,UTL_DRAWELLIPSE,UTL_INSIDEELLIPSE
USE MOD_LEGPLOT, ONLY : LEGPLOT_PLOTUPDATE
USE MOD_IFF, ONLY : UTL_GETUNITIFF,IFFDRAW
USE MOD_MDF, ONLY : READMDF,MDFDEALLOCATE,MDF
USE MOD_TAGS, ONLY : TAGDRAW
USE MOD_MODEL, ONLY : MODEL1DRAW_SIMBOX
USE MOD_IDFTIMESERIE, ONLY : IDFTIMESERIE_PLUSPLOTPOINT
USE MOD_TOPO, ONLY : TOPO1DRAW
USE MOD_SCENTOOL, ONLY : ST1DRAWSCENARIO
USE MOD_SOLID_PROFILE, ONLY : SOLID_PLOTLOCATION_CROSSSECTIONS
USE IMOD, ONLY : IDFDRAW
USE MOD_ISG_PLOT, ONLY : ISGPLOTMAIN
USE MOD_UDF_UTL, ONLY : UDF_OPEN,UDF_PLOTNETWORK,UDF_DEALLOCATEMESH
IMPLICIT NONE
INTEGER,INTENT(IN) :: IPLOTFAST
INTEGER :: IPLOT,IIBITMAP,I,N
REAL :: XMIN,YMIN,XMAX,YMAX
INTEGER,DIMENSION(4) :: IP
LOGICAL :: LPLOT,LEX
CHARACTER(LEN=256) :: FNAME

!INTEGER :: iu,ios,nn,mm,irow,icol
!REAL :: dxe,dye,rat,x,y
!TYPE(IDFOBJ),DIMENSION(3) :: E

CALL WINDOWSELECT(0)

CALL UTL_MESSAGEHANDLE(0)

IF(MPW%IWIN.GT.0)THEN
 IIBITMAP=WINFOBITMAP(MPW%IWIN,BITMAPHANDLE)
ELSE
 IIBITMAP=MPW%IBITMAP
ENDIF

!## create 'mother' bitmap for current coordinates
CALL WBITMAPCREATE(MPW%IBITMAP,MPW%DIX,MPW%DIY)

!## set area/units
CALL UTL_IDFCRDCOR(MPW%XMIN,MPW%XMAX,MPW%YMIN,MPW%YMAX,REAL(MPW%DIX),REAL(MPW%DIY))
CALL IGRPLOTMODE(MODECOPY)

!## define drawable
CALL IDFPLOT1BITMAP()

!## clean canvas
CALL IGRCOLOURN(WRGB(255,255,255))
CALL IGRRECTANGLE(MPW%XMIN,MPW%YMIN,MPW%XMAX,MPW%YMAX)

!## topo-drawing - if not transparant
CALL WINDOWSELECT(0)
IF(WMENUGETSTATE(ID_TOPOGRAPHY,2).EQ.1.AND. &
   WMENUGETSTATE(ID_TOPTRANSPARACY,2).EQ.0)CALL TOPO1DRAW(0,MPW%XMIN,MPW%YMIN,MPW%XMAX,MPW%YMAX,MPW%IBITMAP)

!## polygons
IF(IPLOTFAST.EQ.1)CALL GENDRAW(1)
 
!## collect all other 'child' bitmaps
DRWLIST=0
I      =0
DO IPLOT=1,MXMPLOT

 CALL UTL_FILLARRAY(IP,4,MP(IPLOT)%IDFKIND)
 !## grid colouring
 IF(SUM(IP).EQ.0)CYCLE
 !## temp. turn out contouring/texting
 IF(IPLOTFAST.EQ.0)THEN; IP(2)=0; IP(4)=0; ENDIF
 !## plot for active plot
 IF(.NOT.MP(IPLOT)%ISEL)CYCLE
 !## selected ones only (idf,mdf)
 IF(MP(IPLOT)%IPLOT.NE.1.AND.MP(IPLOT)%IPLOT.NE.5)CYCLE

 !## get idf for mdf file
 LEX=.TRUE.
 IF(MP(IPLOT)%IPLOT.EQ.5)THEN
  FNAME=MP(IPLOT)%IDFNAME
  !## read *.mdf file, only to get selected idf to be plotted
  IF(READMDF(MP(IPLOT)%IDFNAME,N))THEN
   MP(IPLOT)%IDFNAME=MDF(MP(IPLOT)%NLIDF)%FNAME
   CALL MDFDEALLOCATE()
  ELSE
   LEX=.FALSE.
  ENDIF
 ENDIF

 IF(LEX)THEN

  !## reread dimensions ... in case different idf is placed ...
  IF(IDFREAD(MP(IPLOT)%IDF,MP(IPLOT)%IDFNAME,0))THEN

   !## check whether current plot inside current plot-domain
   IF(MP(IPLOT)%IDF%XMIN.LT.MPW%XMAX.AND.MP(IPLOT)%IDF%XMAX.GE.MPW%XMIN.AND. &
      MP(IPLOT)%IDF%YMIN.LT.MPW%YMAX.AND.MP(IPLOT)%IDF%YMAX.GE.MPW%YMIN)THEN

    !## size of coord. fit in plotwindow
    XMIN=MAX(MP(IPLOT)%IDF%XMIN,MPW%XMIN); XMAX=MIN(MP(IPLOT)%IDF%XMAX,MPW%XMAX)
    YMIN=MAX(MP(IPLOT)%IDF%YMIN,MPW%YMIN); YMAX=MIN(MP(IPLOT)%IDF%YMAX,MPW%YMAX)

    LPLOT=.TRUE.
    IF(WMENUGETSTATE(ID_TRANSPARANTIDF,2).EQ.0.AND.   &
       WMENUGETSTATE(ID_TRANSPARANTNODATAIDF,2).EQ.0)THEN
     I=I+1  
     CALL IDFPLOTAREA(XMIN,YMIN,XMAX,YMAX,I,LPLOT)
    ENDIF
    !## plot anyhow
    IF(WMENUGETSTATE(ID_SHOWOPAQUE,2).EQ.1)LPLOT=.TRUE.
    
    !## draw idf in bitmap
     CALL IDFPLOT1BITMAP()
     IF(IDFDRAW(MP(IPLOT)%IDF,MP(IPLOT)%LEG,MP(IPLOT)%UNITS,IP,XMIN,YMIN,XMAX,YMAX, &
        MP(IPLOT)%THICKNESS,LPLOT,UMIN=MP(IPLOT)%UMIN,UMAX=MP(IPLOT)%UMAX))DRWLIST(IPLOT)=1
     !## deallocate idf%x
    CALL IDFDEALLOCATEX(MP(IPLOT)%IDF)
   ENDIF
   CLOSE(MP(IPLOT)%IDF%IU); MP(IPLOT)%IDF%IU=0
  ENDIF
 ENDIF
 IF(MP(IPLOT)%IPLOT.EQ.5)THEN; MP(IPLOT)%IDFNAME=FNAME; ENDIF

ENDDO

CALL IDFPLOT1BITMAP()

!## Topo-drawing
CALL WINDOWSELECT(0)
IF(WMENUGETSTATE(ID_TOPOGRAPHY,2).EQ.1.AND. &
   WMENUGETSTATE(ID_TOPTRANSPARACY,2).EQ.1)CALL TOPO1DRAW(0,MPW%XMIN,MPW%YMIN,MPW%XMAX,MPW%YMAX,MPW%IBITMAP)

IF(IPLOTFAST.EQ.1)THEN

 CALL IGRPLOTMODE(MODECOPY)
 CALL IDFPLOT1BITMAP()

 DO IPLOT=1,MXMPLOT
  IF(MP(IPLOT)%ISEL.AND.MP(IPLOT)%IPLOT.EQ.7)THEN
   !## plot udf
   IF(UDF_OPEN(MP(IPLOT)%IDF,MP(IPLOT)%IDFNAME,1,MP(IPLOT)%IDF%IU))THEN
    CALL UDF_PLOTNETWORK(MP(IPLOT)%IDF%IU,MP(IPLOT)%LEG,MP(IPLOT)%IDF%NODATA,MP(IPLOT)%UMIN,MP(IPLOT)%UMAX)
   ENDIF
   CALL UDF_DEALLOCATEMESH() 
  ENDIF
 ENDDO
 
 !## draw gens
 CALL IGRPLOTMODE(MODECOPY)
 CALL IDFPLOT1BITMAP()
 CALL TOPOGENDRAW(0)

! if(.not.idfread(e(1),'d:\IMOD-MODELS\SWISS\DBASE_VISP_II\ANI\VERSION_2\ELLIPS_RAT.IDF',1))then; endif
! if(.not.idfread(e(2),'d:\IMOD-MODELS\SWISS\DBASE_VISP_II\ANI\VERSION_2\ELLIPS_ANI.IDF',1))then; endif
! if(.not.idfread(e(3),'d:\IMOD-MODELS\SWISS\DBASE_VISP_II\ANI\VERSION_2\ELLIPS_LEN.IDF',1))then; endif
! call igrlinewidth(1)
! call igrlinetype(0)
! do irow=1,e(1)%nrow,25; do icol=1,e(1)%ncol,25
!  call idfgetloc(e(1),irow,icol,x,y)
!  if(e(1)%x(icol,irow).eq.e(1)%nodata)cycle
!  dxe=e(3)%x(icol,irow)/2.0
!  dye=dxe*e(1)%x(icol,irow)
!  rat=e(2)%x(icol,irow)
!  !## 90 for ellips drawing
!  CALL UTL_DRAWELLIPSE(x,y,dxe,dye,rat-90.0) !e(2)%x(icol,irow)-90.0)
! enddo; enddo
!
! if(.not.idfread(e(1),'d:\iMOD-TEST\IMODBATCH_KRIGING\rat.IDF',0))then; endif
! if(.not.idfread(e(2),'d:\iMOD-TEST\IMODBATCH_KRIGING\ANI.IDF',0))then; endif
! if(.not.idfread(e(3),'d:\iMOD-TEST\IMODBATCH_KRIGING\LEN.IDF',0))then; endif
! iu=utl_getunit(); open(iu,file='d:\iMOD-TEST\IMODBATCH_KRIGING\TEST.ipf',status='old',action='read')
! read(iu,*) nn
! read(iu,*) mm
! do i=1,mm+1; read(iu,*) ; enddo
! do i=1,nn
!  read(iu,*,iostat=ios) x,y
!  if(ios.ne.0)exit
!  rat=IDFGETXYVAL(e(2),x,y) !-1928.36,2517.28)
!  dxe=IDFGETXYVAL(e(3),x,y) !-1928.36,2517.28)
!  dye=dxe*IDFGETXYVAL(e(1),x,y) !-1928.36,2517.28)
!  CALL UTL_DRAWELLIPSE(x,y,dxe,dye,rat-90.0) !e(2)%x(icol,irow)-90.0)
! enddo
! close(iu)
 
 !## imod isg plotting!!!
 CALL IGRPLOTMODE(MODECOPY)
 CALL IDFPLOT1BITMAP()
 DO IPLOT=1,MXMPLOT
  IF(MP(IPLOT)%ISEL.AND.MP(IPLOT)%IPLOT.EQ.4)THEN
   CALL IDFPLOT1BITMAP()
   CALL ISGPLOTMAIN(IPLOT,MPW%XMIN,MPW%YMIN,MPW%XMAX,MPW%YMAX)
  ENDIF
 END DO

 !## imodflow-line-files plotting!!!
 CALL IGRPLOTMODE(MODECOPY); 
 CALL IDFPLOT1BITMAP(); CALL IFFDRAW()

 !## imod-point-files plotting!!!
 CALL IGRPLOTMODE(MODECOPY); 
 CALL IDFPLOT1BITMAP(); CALL IPFDRAW() 

 !## esri-gen files plotting!!! 
 CALL IGRPLOTMODE(MODECOPY); 
 CALL IDFPLOT1BITMAP(); CALL GENDRAW(0)

 IF(WMENUGETSTATE(ID_RUNMODEL,2).EQ.1)CALL MODEL1DRAW_SIMBOX(.TRUE.)

 IF(WMENUGETSTATE(ID_IRDATABASE,2).EQ.1)THEN
  !## from previous itree/ifield
  CALL IR1DRAWSHAPES(2)

  !## draw gen-file
  CALL IR1GENDRAW()

 ENDIF

 !peter
 CALL POLYGON1DRAWSHAPE(1,SHPNO)

 !## draw selected points in idftimeserie
 CALL IDFTIMESERIE_PLUSPLOTPOINT()

 !## draw features from the scenario tool
 IF(WMENUGETSTATE(ID_SCENTOOL,2).EQ.1)CALL ST1DRAWSCENARIO()

 !## draw location of cross-sections in case solid modeling is active and profile tool is on!
 IF(WMENUGETSTATE(ID_SOLIDS,2).EQ.1)THEN 
  CALL IDFPLOT1BITMAP()
  CALL IGRPLOTMODE(MODECOPY)
  CALL SOLID_PLOTLOCATION_CROSSSECTIONS()
 ENDIF
 
 !## draw external features
 CALL IGRPLOTMODE(MODECOPY)
 CALL IDFPLOT1BITMAP()
 CALL IDFPLOT_FEATURES()

ENDIF

!## tag-drawing
CALL TAGDRAW()

CALL IDFPLOT2BITMAP()

IF(IIBITMAP.NE.0.AND.IIBITMAP.NE.MPW%IBITMAP)CALL WBITMAPDESTROY(IIBITMAP)

!## legend plotting
CALL LEGPLOT_PLOTUPDATE(.FALSE.)

CALL UTL_MESSAGEHANDLE(1)

RETURN
END SUBROUTINE

!###======================================================================
SUBROUTINE IDFPLOT_CONTOUR(IDF,LEG,IP,THICKNESS)
!###======================================================================
USE WINTERACTER
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_IDF, ONLY : IDFREAD,IDFDEALLOCATEX
USE MODPLOT, ONLY : LEGENDOBJ,CONT
USE MOD_IDF, ONLY : IDFREADPART,UTL_IDFGETCLASS,IDFALLOCATEX 
USE MODPLOT, ONLY : LEGENDOBJ,CONT,NLAB,LABDIST,MPW
USE MOD_UTL, ONLY : RTOS,UTL_SETTEXTSIZE,UTL_REALTOSTRING
IMPLICIT NONE
TYPE(IDFOBJ),INTENT(INOUT) :: IDF
TYPE(LEGENDOBJ),INTENT(INOUT) :: LEG
INTEGER,INTENT(IN),DIMENSION(3) :: IP
INTEGER,INTENT(IN) :: THICKNESS
REAL,ALLOCATABLE,DIMENSION(:) :: DELR,DELC,XC,YC
INTEGER :: I,ICLR
REAL :: TWIDTH,THEIGHT,DXS,DYS,DX
CHARACTER(LEN=15) :: STR

!IF(IDF%IXV.EQ.3)THEN
! IF(ASSOCIATED(IDF%X))DEALLOCATE(IDF%X); ALLOCATE(IDF%X(IDF%NCOL,IDF%NROW))
! DO IROW=1,IDF%NROW; DO ICOL=1,IDF%NCOL
!  I1=IDF%IV(ICOL,IROW,1); I2=IDF%IV(ICOL,IROW,2)
!  I3=IDF%IV(ICOL,IROW,3); I4=IDF%IV(ICOL,IROW,4)
!  F=EXP(REAL(I4)/10.0)
!  IDF%X(ICOL,IROW)=SQRT((REAL(I1)/F)**2.0+(REAL(I2)/F)**2.0+(REAL(I3)/F)**2.0)
! ENDDO; ENDDO
!ENDIF

ALLOCATE(DELR(IDF%NCOL),DELC(IDF%NROW),XC(IDF%NCOL),YC(IDF%NROW))

IF(IDF%IEQ.EQ.0)THEN
 XC(1)=IDF%XMIN+IDF%DX/2.0
 DO I=2,IDF%NCOL; XC(I)=XC(I-1)+IDF%DX; ENDDO
 YC(IDF%NROW)=IDF%YMIN+IDF%DY/2.0
 DO I=IDF%NROW-1,1,-1; YC(I)=YC(I+1)+IDF%DY; ENDDO
 DELR=IDF%DX; DELC=IDF%DY
ELSEIF(IDF%IEQ.EQ.1)THEN
 DO I=1,IDF%NCOL; XC(I)  =(IDF%SX(I-1)+IDF%SX(I))/2.0; ENDDO
 DO I=1,IDF%NCOL; DELR(I)= IDF%SX(I)-IDF%SX(I-1)     ; ENDDO
 DO I=1,IDF%NROW; YC(I)  =(IDF%SY(I-1)+IDF%SY(I))/2.0; ENDDO
 DO I=1,IDF%NROW; DELC(I)= IDF%SY(I-1)-IDF%SY(I)     ; ENDDO
ENDIF

!## allocate memory for labeling
ALLOCATE(CONT(100)); NLAB=0
LABDIST=0.10*SQRT((MPW%YMAX-MPW%YMIN)**2.0+(MPW%XMAX-MPW%XMIN)**2.0)

#if(defined(WINTERACTER11))
 CALL IGRLINECAP(ROUNDCAP)
 CALL IGRLINEJOIN(ROUNDJOIN)
#endif

DO I=0,LEG%NCLR
 CALL IGRLINEWIDTH(MAX(1,ABS(THICKNESS)))
 IF(MOD(I,5).EQ.0)CALL IGRLINEWIDTH(MAX(ABS(THICKNESS)-1,1))
 IF(IP(1).EQ.0)THEN
  ICLR=UTL_IDFGETCLASS(LEG,LEG%CLASS(I))
 ELSE
  ICLR=WRGB(0,0,0) 
 ENDIF 
 CALL IGRCOLOURN(ICLR)
 CALL IDFPLOT_COMPCONTOUR(IDF%X,IDF%NCOL,IDF%NROW,XC,YC,LEG%CLASS(I),IDF%NODATA,THICKNESS)
ENDDO

#if(defined(WINTERACTER11))
 CALL IGRLINEJOIN()
 CALL IGRLINECAP()
#endif

IF(THICKNESS.GT.0)THEN
 CALL UTL_SETTEXTSIZE(TWIDTH,THEIGHT,REAL(THICKNESS)*0.01) 
 CALL WGRTEXTFONT(FFHELVETICA,WIDTH=TWIDTH,HEIGHT=THEIGHT,ISTYLE=0)
 DXS=WINFOGRREAL(GRAPHICSCHWIDTH)
 DYS=WINFOGRREAL(GRAPHICSCHHEIGHT)

 CALL IGRFILLPATTERN(SOLID)
 DO I=1,NLAB
  STR=UTL_REALTOSTRING(CONT(I)%VLAB)
  DX=WGRTEXTLENGTH(' '//TRIM(STR)//' ',0)*DXS
  CALL IDFPLOT_CLEANBOX(CONT(I)%XLAB,CONT(I)%YLAB,CONT(I)%ALAB,DX,DYS*0.8,WRGB(255,255,255)) 
  CALL WGRTEXTORIENTATION(ALIGNCENTRE,ANGLE=CONT(I)%ALAB) 
  CALL IGRCOLOURN(WRGB(0,0,0))
  CALL WGRTEXTSTRING(CONT(I)%XLAB,CONT(I)%YLAB,TRIM(STR))
 ENDDO
ENDIF

CALL IGRFILLPATTERN(OUTLINE)

IF(ASSOCIATED(CONT))DEALLOCATE(CONT); NLAB=0
DEALLOCATE(DELR,DELC,XC,YC)

END SUBROUTINE IDFPLOT_CONTOUR

!###======================================================================
SUBROUTINE IDFPLOT_CLEANBOX(X,Y,ANGLE,DX,DY,ICLR)
!###======================================================================
USE IMODVAR, ONLY : PI
IMPLICIT NONE
REAL,INTENT(IN) :: X,Y,ANGLE,DX,DY
INTEGER,INTENT(IN) :: ICLR
REAL,DIMENSION(6) :: XC,YC
REAL :: RAD

IF(DX.LE.0.0)RETURN
CALL IGRCOLOURN(ICLR)

RAD=ANGLE/(360.0/(2.0*PI))
XC(6)=X-(COS(RAD)*0.5*DX); YC(6)=Y-(SIN(RAD)*0.5*DX)
XC(5)=X+(COS(RAD)*0.5*DX); YC(5)=Y+(SIN(RAD)*0.5*DX)

RAD=(ANGLE+90.0)/(360.0/(2.0*PI))
XC(1)=XC(6)-(COS(RAD)*0.5*DY); YC(1)=YC(6)-(SIN(RAD)*0.5*DY)
XC(2)=XC(5)-(COS(RAD)*0.5*DY); YC(2)=YC(5)-(SIN(RAD)*0.5*DY)
XC(3)=XC(5)+(COS(RAD)*0.5*DY); YC(3)=YC(5)+(SIN(RAD)*0.5*DY)
XC(4)=XC(6)+(COS(RAD)*0.5*DY); YC(4)=YC(6)+(SIN(RAD)*0.5*DY)

CALL IGRPOLYGONCOMPLEX(XC,YC,4)

END SUBROUTINE IDFPLOT_CLEANBOX

!###======================================================================
SUBROUTINE IDFPLOT_PUTLABEL(X1,Y1,X2,Y2,V,INILAB,XMIN,YMIN,XMAX,YMAX)
!###======================================================================
USE MODPLOT, ONLY : LEGENDOBJ,CONT,CONT_BU,NLAB,LABDIST !,MPW
USE IMODVAR, ONLY : PI
IMPLICIT NONE
REAL,INTENT(IN) :: X1,Y1,X2,Y2,V,XMIN,YMIN,XMAX,YMAX
INTEGER,INTENT(INOUT) :: INILAB
INTEGER :: IOK,I
REAL :: X12,Y12,D,DE

!!## if segment too short do not place a label
!D=(X1-X2)**2.0+(Y1-Y2)**2.0; IF(D.GT.0.0)D=SQRT(D)
!IF(D.LT.LABDIST/10.0)RETURN

X12=(X1+X2)/2.0; Y12=(Y1+Y2)/2.0
IOK=1; IF(NLAB.GT.0)THEN
 DO I=1,NLAB
  D =(X12-CONT(I)%XLAB)**2.0+(Y12-CONT(I)%YLAB)**2.0; IF(D.GT.0.0)D=SQRT(D)
  IF(V.EQ.CONT(I)%VLAB)THEN
   !## to close to another label
   IF(D.LT.LABDIST)THEN; IOK=0; EXIT; ENDIF
  ELSE
   !## to close to another label
   IF(D.LT.LABDIST/5.0)THEN; IOK=0; EXIT; ENDIF
  ENDIF
 ENDDO
!## first label not too close to edge of graphical canvas
ENDIF
!## take edge into account
IF(INILAB.EQ.0)THEN
 DE=MIN(ABS(X12-XMIN),ABS(X12-XMAX),ABS(Y12-YMIN),ABS(Y12-YMAX))
 IF(DE.LT.LABDIST/2.0)IOK=0
ENDIF

IF(IOK.EQ.0)RETURN
INILAB=INILAB+1

IF(NLAB+1.GT.SIZE(CONT))THEN
 ALLOCATE(CONT_BU(NLAB+100))
 CONT_BU(1:NLAB)=CONT(1:NLAB)
 DEALLOCATE(CONT); CONT=>CONT_BU
ENDIF

NLAB=NLAB+1
CONT(NLAB)%XLAB=X12 
CONT(NLAB)%YLAB=Y12 
!## store angle
IF(X1.NE.X2) THEN
 CONT(NLAB)%ALAB=ATAN((Y1-Y2)/(X1-X2))/PI*180.0
ELSE
 CONT(NLAB)%ALAB=90.0
ENDIF
!## store label-value
CONT(NLAB)%VLAB=V

END SUBROUTINE IDFPLOT_PUTLABEL

!###======================================================================
SUBROUTINE IDFPLOT_COMPCONTOUR(XVAL,NCOL,NROW,XC,YC,V,XNODATA,THICKNESS)
!###======================================================================
USE MODPLOT, ONLY : MPW
IMPLICIT NONE
INTEGER,INTENT(IN) :: NCOL,NROW,THICKNESS
REAL,INTENT(IN) :: XNODATA,V
REAL,INTENT(IN),DIMENSION(NCOL,NROW) :: XVAL
REAL,INTENT(IN),DIMENSION(NCOL) :: XC
REAL,INTENT(IN),DIMENSION(NROW) :: YC
REAL,DIMENSION(8) :: XS,YS
INTEGER :: I,J,II,INILAB,IROW,ICOL
REAL :: XMIN,YMIN,XMAX,YMAX,DX,DY,DA,DV
REAL,DIMENSION(4) :: A
REAL,DIMENSION(4) :: X,Y
INTEGER,DIMENSION(2,8) :: POS
DATA POS/1,3,1,2,2,4,2,3,3,1,3,4,4,2,4,1/

INILAB=0
XMIN=MAX(MPW%XMIN,XC(1)); XMAX=MIN(MPW%XMAX,XC(NCOL))
YMIN=MAX(MPW%YMIN,YC(NROW)); YMAX=MIN(MPW%YMAX,YC(1))

DO ICOL=1,NCOL-1
 DO IROW=2,NROW

  A(1)=XVAL(ICOL,IROW); A(2)=XVAL(ICOL+1,IROW); A(3)=XVAL(ICOL+1,IROW-1); A(4)=XVAL(ICOL,IROW-1)

  !## skip flat areas
  J=0; DO I=1,4; IF(A(I).EQ.V)J=J+1; ENDDO; IF(J.EQ.4)CYCLE

  !## skip nodata areas  
  J=0; DO I=1,4; IF(A(I).EQ.XNODATA)J=J+1; ENDDO; IF(J.NE.0)CYCLE 

  !## midpoints
  X(1)=XC(ICOL); X(2)=XC(ICOL+1); X(3)=X(2); X(4)=X(1)
  Y(1)=YC(IROW); Y(2)=Y(1); Y(3)=YC(IROW-1); Y(4)=Y(3)
    
  !## set contour-line
  II=0
  DO I=2,8,2 !1,8 !2,8,2
   IF((V.GE.A(POS(1,I)).AND.V.LT.A(POS(2,I))).OR. &
      (V.GE.A(POS(2,I)).AND.V.LT.A(POS(1,I))))THEN
        
    DA=A(POS(2,I))-A(POS(1,I))
    DV=V-A(POS(1,I))
    DX=X(POS(2,I))-X(POS(1,I))
    DY=Y(POS(2,I))-Y(POS(1,I))

!    !## diagonal if over half, skip it
!    IF(MOD(I,2).NE.0)THEN
!     IF(ABS(DV/DA).GT.0.5)CYCLE
!    ENDIF
    
    II=II+1
    XS(II)=X(POS(1,I))+DV/DA*DX
    YS(II)=Y(POS(1,I))+DV/DA*DY

   ENDIF
  ENDDO
  
  DO I=1,II-1; CALL IGRJOIN(XS(I),YS(I),XS(I+1),YS(I+1)); ENDDO
  !## add label
  IF(II.GT.1.AND.THICKNESS.GT.0)CALL IDFPLOT_PUTLABEL(XS(1),YS(1),XS(2),YS(2),V,INILAB,XMIN,YMIN,XMAX,YMAX)

 ENDDO
ENDDO

END SUBROUTINE IDFPLOT_COMPCONTOUR
       
!###======================================================================
SUBROUTINE IDFPLOT_FEATURES()
!###======================================================================
USE WINTERACTER
USE RESOURCE
USE MODPLOT
IMPLICIT NONE
REAL :: XP1,YP1,XP2,YP2,DX,DY

CALL WINDOWSELECT(0)
IF(WMENUGETSTATE(ID_IDFRASTERLINES,2).EQ.1)CALL IDFPLOT_FEATURES_RASTER()
!IF(WMENUGETSTATE(ID_IDFEXTENT,2).EQ.1)     
CALL IDFPLOT_FEATURES_EXTENT()
!IF(WMENUGETSTATE(ID_IDFINDICES,2).EQ.1)    CALL IDFPLOT_FEATURES_EXTENT()
IF(WMENUGETSTATE(ID_SHOWSCALEBAR,2).EQ.1)THEN
 XP1=0.75 !0.6
 YP1=0.075
 XP2=0.95 !1.0
 YP2=0.175
 DX =MPW%XMAX-MPW%XMIN
 DY =MPW%YMAX-MPW%YMIN
 CALL IGRAREA(XP1,YP1,XP2,YP2)
 CALL IGRUNITS(MPW%XMIN+XP1*DX,MPW%YMIN+YP1*DY,MPW%XMIN+XP2*DX,MPW%YMIN+YP2*DY)
 CALL IDFPLOT_FEATURES_SCALE()
 CALL IGRAREA(0.0,0.0,1.0,1.0)
 CALL IGRUNITS(MPW%XMIN,MPW%YMIN,MPW%XMAX,MPW%YMAX)
ENDIF
IF(WMENUGETSTATE(ID_SHOWNARROW,2).EQ.1)CALL IDFPLOT_FEATURES_NARROW()
IF(WMENUGETSTATE(ID_SHOWAXES,2).EQ.1)  CALL IDFPLOT_FEATURES_AXES()

END SUBROUTINE IDFPLOT_FEATURES

!###======================================================================
SUBROUTINE IDFPLOT_FEATURES_AXES()
!###======================================================================
USE WINTERACTER
USE MODPLOT
USE MOD_UTL, ONLY : UTL_GETAXESCALES,UTL_GETFORMAT,NSX,NSY,SXVALUE,SYVALUE,UTL_EQUALS_REAL
IMPLICIT NONE
REAL :: R1,R2,R3
INTEGER,PARAMETER :: N=10
INTEGER :: I,J
REAL :: DX,DY,V1,VI,X1,X2,Y1,Y2,R4,TWIDTH,THEIGHT
REAL :: DXX,OX1,OY1,OX2,OY2,RAT
CHARACTER(LEN=10) :: FRM

R1=2.5  !% percentage of total border around plot (white)
R2=2.0  !% part occupied by axes (blue)
R3=0.35 !% of area to be used for axes (other area occupied by text)

OX1=WINFOGRREAL(GRAPHICSUNITMINX)! (7) LEFT LIMIT OF MAIN GRAPHICS AREA
OY1=WINFOGRREAL(GRAPHICSUNITMINY)! (8) LOWER LIMIT OF MAIN GRAPHICS AREA
OX2=WINFOGRREAL(GRAPHICSUNITMAXX)! (9) RIGHT LIMIT OF MAIN GRAPHICS AREA
OY2=WINFOGRREAL(GRAPHICSUNITMAXY)! (10) UPPER LIMIT OF

RAT=(OX2-OX1)/(OY2-OY1)

IF(RAT.GT.1.0)THEN
 R1=R1*RAT
 R2=R2*RAT
 R3=R3*RAT
ELSEIF(RAT.LT.1.0)THEN
 R1=R1/RAT
 R2=R2/RAT
 R3=R3/RAT
ENDIF

!## textsize in graphical dimensions
R4=R2-(2.0*R3)
!## textsize (percentage)
R4=R4/100.0 

IF(RAT.GE.1.0)THEN
 THEIGHT=R4
 TWIDTH =THEIGHT/(0.03333/0.01333)/RAT 
ELSEIF(RAT.LT.1.0)THEN
 THEIGHT=R4*RAT 
 TWIDTH =THEIGHT/(0.03333/0.01333)/RAT
ENDIF
!CALL WGRTEXTFONT(FFHELVETICA,WIDTH=TWIDTH,HEIGHT=THEIGHT,ISTYLE=0)

!## white
DXX=MIN((MPW%XMAX-MPW%XMIN),(MPW%YMAX-MPW%YMIN))
DX=DXX*(R1/100.0); DY=DX

!DO
! IF(.NOT.UTL_EQUALS_REAL(MPW%XMIN+DX,MPW%XMIN))EXIT
! DX=DX*2.0
!ENDDO
!DO
! IF(.NOT.UTL_EQUALS_REAL(MPW%YMIN+DY,MPW%YMIN))EXIT
! DY=DY*2.0
!ENDDO

CALL IGRFILLPATTERN(SOLID)
CALL IGRCOLOURN(WRGB(255,255,255))
CALL IGRRECTANGLE(MPW%XMIN,MPW%YMIN,MPW%XMIN+DX,MPW%YMAX)
CALL IGRRECTANGLE(MPW%XMIN,MPW%YMIN,MPW%XMAX,MPW%YMIN+DY)
CALL IGRRECTANGLE(MPW%XMIN,MPW%YMAX-DY,MPW%XMAX,MPW%YMAX)
CALL IGRRECTANGLE(MPW%XMAX-DX,MPW%YMIN,MPW%XMAX,MPW%YMAX)

!## wit - blank out axes-area
DX=DXX*(R2/100.0); DY=DX

!## bounding box
CALL IGRFILLPATTERN(OUTLINE); CALL IGRLINEWIDTH(2); CALL IGRCOLOURN(WRGB(0,0,0))
CALL IGRRECTANGLE(MPW%XMIN+DX,MPW%YMIN+DY,MPW%XMAX-DX,MPW%YMAX-DY)

!CALL WGRTEXTORIENTATION(IALIGN=ALIGNCENTRE,ANGLE=0.0,NALIGN=ALIGNCENTRE)

Y1=(MPW%YMIN+MPW%YMAX)/2.0-(MPW%XMAX-MPW%XMIN)/2.0
Y2=(MPW%YMIN+MPW%YMAX)/2.0+(MPW%XMAX-MPW%XMIN)/2.0
CALL UTL_GETAXESCALES(MPW%XMIN+DX,Y1+DX,MPW%XMAX-DX,Y2-DX) 

!## x-axes interval
VI=SXVALUE(2)-SXVALUE(1)

Y1=MPW%YMIN+DXX*(R2/100.0)
Y2=MPW%YMAX-DXX*(R2/100.0)
DY=DXX*(R3/100.0)

CALL WGRTEXTFONT(FFHELVETICA,WIDTH=TWIDTH/2.0,HEIGHT=THEIGHT/2.0,ISTYLE=0)
CALL WGRTEXTORIENTATION(IALIGN=ALIGNCENTRE,ANGLE=0.0,NALIGN=ALIGNCENTRE)

!## minor ticks along x-axes
CALL IGRLINEWIDTH(1)
!## check whether interval is large enough
DO
 IF(.NOT.UTL_EQUALS_REAL(SXVALUE(1)-VI/4.0,SXVALUE(1)))EXIT
 VI=VI*2.0
ENDDO
V1=SXVALUE(1)-VI
I=0
DO
 I=I+1
 V1=V1+VI/4.0
 IF(V1.GT.MPW%XMAX-DXX*(R1/100.0))EXIT
 IF(V1.GT.MPW%XMIN+DXX*(R1/100.0))THEN
  CALL IGRJOIN(V1,Y1-DY/2.0,V1,Y1+DY/2.0)
  CALL IGRJOIN(V1,Y2-DY/2.0,V1,Y2+DY/2.0)

  IF(MOD(I,4).NE.0.AND.MOD(I,2).EQ.0)THEN
   FRM=TRIM(UTL_GETFORMAT(V1/1000.0))
   J=INDEX(FRM,'.0)',.TRUE.)
   IF(J.EQ.0)THEN
    CALL WGRTEXTREAL(V1,Y1-3.0*DY,V1/1000.0,FRM)
    CALL WGRTEXTREAL(V1,Y2+3.0*DY,V1/1000.0,FRM)
   ELSE
    CALL WGRTEXTINTEGER(V1,Y1-3.0*DY,INT(V1/1000.0))
    CALL WGRTEXTINTEGER(V1,Y2+3.0*DY,INT(V1/1000.0))
   ENDIF
  ENDIF

 ENDIF
END DO

CALL WGRTEXTFONT(FFHELVETICA,WIDTH=TWIDTH,HEIGHT=THEIGHT,ISTYLE=0)
CALL WGRTEXTORIENTATION(IALIGN=ALIGNCENTRE,ANGLE=0.0,NALIGN=ALIGNCENTRE)

!## major ticks
CALL IGRLINEWIDTH(2)
V1=SXVALUE(1)-VI
DO
 V1=V1+VI
 IF(V1.GT.MPW%XMAX-DXX*(R1/100.0))EXIT
 IF(V1.GT.MPW%XMIN+DXX*(R1/100.0))THEN
  CALL IGRJOIN(V1,Y1-DY,V1,Y1+DY)
  CALL IGRJOIN(V1,Y2-DY,V1,Y2+DY)
  FRM=TRIM(UTL_GETFORMAT(V1/1000.0))
  J=INDEX(FRM,'.0)',.TRUE.)
  IF(J.EQ.0)THEN
   CALL WGRTEXTREAL(V1,Y1-3.0*DY,V1/1000.0,FRM)
   CALL WGRTEXTREAL(V1,Y2+3.0*DY,V1/1000.0,FRM)
  ELSE
   CALL WGRTEXTINTEGER(V1,Y1-3.0*DY,INT(V1/1000.0))
   CALL WGRTEXTINTEGER(V1,Y2+3.0*DY,INT(V1/1000.0))
  ENDIF
 ENDIF
END DO

!## y-axes

!## minor ticks

CALL WGRTEXTFONT(FFHELVETICA,WIDTH=TWIDTH/2.0,HEIGHT=THEIGHT/2.0,ISTYLE=0)
CALL WGRTEXTORIENTATION(IALIGN=ALIGNCENTRE,ANGLE=90.0,NALIGN=ALIGNCENTRE)

!## y-axes interval
VI=SYVALUE(2)-SYVALUE(1)

X1=MPW%XMIN+DXX*(R2/100.0)
X2=MPW%XMAX-DXX*(R2/100.0)
DX=DXX*(R3/100.0) 
CALL IGRLINEWIDTH(1)
!## check whether interval is large enough
DO
 IF(.NOT.UTL_EQUALS_REAL(SYVALUE(1)-VI/4.0,SYVALUE(1)))EXIT
 VI=VI*2.0
ENDDO

V1=SYVALUE(1)-VI
I=0
DO
 I=I+1
 V1=V1+VI/4.0
 IF(V1.GT.MPW%YMAX-DXX*(R1/100.0))EXIT
 IF(V1.GT.MPW%YMIN+DXX*(R1/100.0))THEN
  CALL IGRJOIN(X1-DX/2.0,V1,X1+DX/2.0,V1)
  CALL IGRJOIN(X2-DX/2.0,V1,X2+DX/2.0,V1)
  
  IF(MOD(I,4).NE.0.AND.MOD(I,2).EQ.0)THEN
   FRM=TRIM(UTL_GETFORMAT(V1/1000.0))
   J=INDEX(FRM,'.0)',.TRUE.)
   IF(J.EQ.0)THEN
    CALL WGRTEXTREAL(X1-3.0*DX,V1,V1/1000.0,FRM)
    CALL WGRTEXTREAL(X2+3.0*DX,V1,V1/1000.0,FRM)
   ELSE
    CALL WGRTEXTINTEGER(X1-3.0*DX,V1,INT(V1/1000.0))
    CALL WGRTEXTINTEGER(X2+3.0*DX,V1,INT(V1/1000.0))
   ENDIF
  ENDIF
 
 ENDIF
END DO

!## major ticks

CALL WGRTEXTFONT(FFHELVETICA,WIDTH=TWIDTH,HEIGHT=THEIGHT,ISTYLE=0)

CALL IGRLINEWIDTH(2)
V1=SYVALUE(1)-VI
DO
 V1=V1+VI
 IF(V1.GT.MPW%YMAX-DXX*(R1/100.0))EXIT
 IF(V1.GT.MPW%YMIN+DXX*(R1/100.0))THEN
  CALL IGRJOIN(X1-DX,V1,X1+DX,V1)
  CALL IGRJOIN(X2-DX,V1,X2+DX,V1)
  FRM=TRIM(UTL_GETFORMAT(V1/1000.0))
  J=INDEX(FRM,'.0)',.TRUE.)
  IF(J.EQ.0)THEN
   CALL WGRTEXTREAL(X1-3.0*DX,V1,V1/1000.0,FRM)
   CALL WGRTEXTREAL(X2+3.0*DX,V1,V1/1000.0,FRM)
  ELSE
   CALL WGRTEXTINTEGER(X1-3.0*DX,V1,INT(V1/1000.0))
   CALL WGRTEXTINTEGER(X2+3.0*DX,V1,INT(V1/1000.0))
  ENDIF
 ENDIF
END DO

CALL WGRTEXTORIENTATION(IALIGN=ALIGNLEFT,ANGLE=0.0)
CALL IGRLINEWIDTH(1)

END SUBROUTINE IDFPLOT_FEATURES_AXES

!###======================================================================
SUBROUTINE IDFPLOT_FEATURES_SCALE()
!###======================================================================
USE WINTERACTER
USE RESOURCE
USE IMODVAR, ONLY : IMOD_IUNITS
USE MOD_UTL, ONLY : UTL_GETAXESCALES,UTL_GETFORMAT,SXVALUE,SYVALUE,NSX,NSY,UTL_EQUALS_REAL
IMPLICIT NONE
REAL :: WX1,WY1,WX2,WY2
REAL :: X,X1,X2,XI,TWIDTH,THEIGHT,Y,DY,XT
INTEGER :: N,I,J,II
CHARACTER(LEN=10) :: FRM
CHARACTER(LEN=20) :: LINE
INTEGER,DIMENSION(2,2) :: ICLR
REAL :: RAT,XFCT

WX1=WINFOGRREAL(GRAPHICSUNITMINX)! (7) LEFT LIMIT OF MAIN GRAPHICS AREA
WY1=WINFOGRREAL(GRAPHICSUNITMINY)! (8) LOWER LIMIT OF MAIN GRAPHICS AREA
WX2=WINFOGRREAL(GRAPHICSUNITMAXX)! (9) RIGHT LIMIT OF MAIN GRAPHICS AREA
WY2=WINFOGRREAL(GRAPHICSUNITMAXY)! (10) UPPER LIMIT OF
RAT=(WX2-WX1)/(WY2-WY1)

X1=0.0; X2=(WX2-WX1)/1.25 
CALL UTL_GETAXESCALES(X1,0.0,X2,1.0)

XI=SXVALUE(2)-SXVALUE(1)
IF(NSX.GT.8)THEN; NSX=NSX/2; XI=XI*2.0; ENDIF
N=NSX

!## mid
X1=(WX2+WX1)/2.0-((REAL(N)*XI)/2.0)
X2=X1+REAL(N)*XI

DY=(WY2-WY1)/6.0 

ICLR(1,1)=WRGB(0,0,0)
ICLR(2,1)=WRGB(255,255,255)
ICLR(1,2)=WRGB(255,255,255)
ICLR(2,2)=WRGB(0,0,0)
CALL IGRFILLPATTERN(SOLID)

Y=WY1+DY*4.0
CALL IGRFILLPATTERN(SOLID)
DO II=1,2
 IF(II.EQ.2)Y=Y-(0.5*DY)
 DO I=1,N
  IF(MOD(I,2).EQ.0)CALL IGRCOLOURN(ICLR(1,II))
  IF(MOD(I,2).NE.0)CALL IGRCOLOURN(ICLR(2,II))
  X =X1+(I-1)*XI
  X2=X1+(I)*XI
  IF(.NOT.UTL_EQUALS_REAL(Y-0.5*DY,Y))CALL IGRRECTANGLE(X,Y-0.5*DY,X2,Y)
 ENDDO
 !## first, split in 10
 DO I=1,10
  IF(MOD(I,2).EQ.0)CALL IGRCOLOURN(ICLR(1,II))
  IF(MOD(I,2).NE.0)CALL IGRCOLOURN(ICLR(2,II))
  X =X1+(I-1)*(XI/10.0)
  X2=X1+I*(XI/10.0)
  IF(.NOT.UTL_EQUALS_REAL(Y-0.5*DY,Y))CALL IGRRECTANGLE(X,Y-0.5*DY,X2,Y)
 ENDDO
 !## second, split in 2
 DO I=1,2
  IF(MOD(I,2).EQ.0)CALL IGRCOLOURN(ICLR(1,II))
  IF(MOD(I,2).NE.0)CALL IGRCOLOURN(ICLR(2,II))
  X =X1+XI+(I-1)*(XI/2.0)
  X2=X1+XI+I*(XI/2.0)
  IF(.NOT.UTL_EQUALS_REAL(Y-0.5*DY,Y))CALL IGRRECTANGLE(X,Y-0.5*DY,X2,Y)
 ENDDO
END DO

CALL IGRCOLOURN(WRGB(0,0,0))
CALL IGRFILLPATTERN(OUTLINE)
X1=(WX2+WX1)/2.0-((REAL(N)*XI)/2.0)
X2=X1+REAL(N)*XI
Y=WY1+DY*4
CALL IGRRECTANGLE(X1,Y-DY,X2,Y)
CALL IGRJOIN     (X1,Y-0.5*DY,X2,Y-0.5*DY)

I=INT(LOG10(WX2-WX1))

SELECT CASE (IMOD_IUNITS)
 CASE (0,1)
  SELECT CASE (I)
   CASE (6:);  XFCT=1000000.0
   CASE (3:5); XFCT=1000.0 !## km
   CASE (:2);  XFCT=1.0    !## meter
  END SELECT
 CASE (2)
  SELECT CASE (I)
   CASE (6:);  XFCT=5280000.0 !## 1000 mile
   CASE (3:5); XFCT=5280.0    !## mile
   CASE (:2);  XFCT=1.0       !## feet
  END SELECT
END SELECT

CALL IGRLINEWIDTH(1)
CALL IGRFILLPATTERN(OUTLINE)
!## textsize in graphical dimensions
THEIGHT=0.25 
TWIDTH =THEIGHT/(0.03333/0.01333)/RAT
CALL WGRTEXTFONT(FFHELVETICA,WIDTH=TWIDTH,HEIGHT=THEIGHT,ISTYLE=0)
CALL WGRTEXTORIENTATION(IALIGN=ALIGNCENTRE,ANGLE=0.0)
!## vertical lines
Y =WY1+DY*3.0
XT=0.0
DO I=1,N+1
 X =X1+(I-1)*XI
 XT=(I-1)*XI
 CALL IGRCOLOURN(WRGB(0,0,0))
 CALL IGRJOIN(X,Y-0.5*DY,X,Y+DY )
 FRM=TRIM(UTL_GETFORMAT(XT/XFCT))
 J=INDEX(FRM,'.0)',.TRUE.)
 IF(J.EQ.0)THEN
  CALL IREALTOSTRING(XT/XFCT,LINE,FRM)
 ELSE
  CALL INTEGERTOSTRING(INT(XT/XFCT),LINE,'(I10)')
 ENDIF
 LINE=ADJUSTL(LINE)
 CALL WGRTEXTSTRING(X,WY1+1.75*DY,TRIM(LINE))
END DO
Y =WY1+DY*5.0
CALL WGRTEXTFONT(FFHELVETICA,WIDTH=TWIDTH,HEIGHT=THEIGHT,ISTYLE=0)

SELECT CASE (IMOD_IUNITS)
 CASE (0,1) !## meters
  IF(XFCT.EQ.1000000.0)CALL WGRTEXTSTRING((WX1+WX2)/2.0,Y,'kilometer (*1000)')
  IF(XFCT.EQ.1000.0)CALL WGRTEXTSTRING((WX1+WX2)/2.0,Y,'kilometer')
  IF(XFCT.EQ.1.0)CALL WGRTEXTSTRING((WX1+WX2)/2.0,Y,'meters')
 CASE (2) !## feet
  IF(XFCT.EQ.5280000.0)CALL WGRTEXTSTRING((WX1+WX2)/2.0,Y,'mile (*1000)')
  IF(XFCT.EQ.5280.0)CALL WGRTEXTSTRING((WX1+WX2)/2.0,Y,'mile')
  IF(XFCT.EQ.1.0)CALL WGRTEXTSTRING((WX1+WX2)/2.0,Y,'feet')
END SELECT

END SUBROUTINE IDFPLOT_FEATURES_SCALE

!###======================================================================
SUBROUTINE IDFPLOT_FEATURES_NARROW()
!###======================================================================
USE WINTERACTER
USE RESOURCE
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MODPLOT, ONLY : MPW
USE MOD_UTL, ONLY : UTL_LOADIMAGE
IMPLICIT NONE
REAL :: X1,Y2,XOFFSET,IXDES1,IYDES1,IXDES2,IYDES2,RAT1,RAT2
INTEGER,DIMENSION(3) :: INFO
INTEGER,ALLOCATABLE,DIMENSION(:,:) :: IBMPDATA
INTEGER :: IBITMAP

!# appropriate keyword not available
IF(LEN_TRIM(PREFVAL(11)).EQ.0)RETURN

IF(WMENUGETSTATE(ID_SHOWAXES,2).EQ.1)XOFFSET=0.05
IF(WMENUGETSTATE(ID_SHOWAXES,2).EQ.0)XOFFSET=0.025

RAT1=WINFOGRREAL(GRAPHICSRATIO) !## dx/dy

X1=XOFFSET!0.05
Y2=1.0-XOFFSET*RAT1!0.95

!CALL WGRCURVE(X,Y,3)
!BMPFNAME='D:\IMOD-CODE\IMOD-GUI\BMP\NORTH_ARROW.PNG'
CALL IGRFILEINFO(PREFVAL(11),INFO,3)
!## file not found
IF(INFO(1).EQ.0)THEN
 CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot find:'//CHAR(13)// &
  TRIM(PREFVAL(11)),'Warning')
 PREFVAL(11)=''
 RETURN
ENDIF
IF(INFO(1).LT.0)THEN
 CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Wrong format for:'//CHAR(13)// &
  TRIM(PREFVAL(11)),'Warning')
 PREFVAL(11)=''
 RETURN
ENDIF

IF(ALLOCATED(IBMPDATA))DEALLOCATE(IBMPDATA)
ALLOCATE(IBMPDATA(INFO(2),INFO(3)))

IF(.NOT.UTL_LOADIMAGE(PREFVAL(11),SIZE(IBMPDATA),IBMPDATA,0))THEN
 DEALLOCATE(IBMPDATA); RETURN
ENDIF

CALL WBITMAPCREATE(IBITMAP,INFO(2),INFO(3))
CALL WBITMAPGETDATA(IBITMAP,IBMPDATA)

RAT2=REAL(INFO(3))/REAL(INFO(2)) !## dy/dx

!## largest side = 0.1
IF(INFO(2).GT.INFO(3))THEN !## col>row
 IXDES1=X1
 IXDES2=X1+0.1
 IYDES1=Y2-(0.1*RAT1*RAT2)
 IYDES2=Y2
 Y2    =0.005*RAT1
 X1    =0.005
ELSE                       !## row>col
 IXDES1=X1
 IXDES2=X1+(0.1/RAT1/RAT2)
 IYDES1=Y2-0.1
 IYDES2=Y2
 X1    =0.005/RAT1
 Y2    =0.005
ENDIF

CALL IGRAREA(IXDES1-X1,IYDES1-Y2,IXDES2+X1,IYDES2+Y2)
CALL IGRFILLPATTERN(SOLID)
CALL IGRCOLOURN(WRGB(255,255,255))
CALL IGRUNITS(0.0,0.0,1.0,1.0)
CALL IGRRECTANGLE(0.0,0.0,1.0,1.0)
CALL IGRAREA(IXDES1,IYDES1,IXDES2,IYDES2)
CALL WBITMAPPUT(IBITMAP,2,1)
CALL WBITMAPDESTROY(IBITMAP)

END SUBROUTINE IDFPLOT_FEATURES_NARROW

!###======================================================================
SUBROUTINE IDFPLOT_FEATURES_RASTER()
!###======================================================================
USE WINTERACTER
USE RESOURCE
USE MODPLOT,ONLY : MP,MXMPLOT
USE MOD_IDF, ONLY : IDFREAD,IDFDEALLOCATEX
USE MOD_UDF_UTL, ONLY : UDF_OPEN,UDF_PLOTNETWORK
USE MOD_MDF, ONLY : READMDF,MDFDEALLOCATE,MDF
IMPLICIT NONE
INTEGER :: IPLOT,IC,IR,N
LOGICAL :: LEX
CHARACTER(LEN=256) :: FNAME

CALL IGRCOLOURN(WRGB(100,100,100))
CALL IGRLINEWIDTH(1)
DO IPLOT=1,MXMPLOT

 IF(.NOT.MP(IPLOT)%ISEL)CYCLE
 
 IF(MP(IPLOT)%IPLOT.EQ.1.OR.MP(IPLOT)%IPLOT.EQ.5)THEN

  !## get idf for mdf file
  LEX=.TRUE.
  IF(MP(IPLOT)%IPLOT.EQ.5)THEN
   FNAME=MP(IPLOT)%IDFNAME
   !## read *.mdf file, only to get selected idf to be plotted
   IF(READMDF(MP(IPLOT)%IDFNAME,N))THEN
    MP(IPLOT)%IDFNAME=MDF(MP(IPLOT)%NLIDF)%FNAME
    CALL MDFDEALLOCATE()
   ELSE
    LEX=.FALSE.
   ENDIF
  ENDIF

  IF(LEX)THEN

   !## reread dimensions ... in case different idf is placed ...
   IF(IDFREAD(MP(IPLOT)%IDF,MP(IPLOT)%IDFNAME,0))THEN

    IF(MP(IPLOT)%IDF%IEQ.EQ.0)THEN

     DO IR=0,MP(IPLOT)%IDF%NROW
      CALL IGRJOIN(MP(IPLOT)%IDF%XMIN,MP(IPLOT)%IDF%YMAX-IR*MP(IPLOT)%IDF%DY, &
                   MP(IPLOT)%IDF%XMAX,MP(IPLOT)%IDF%YMAX-IR*MP(IPLOT)%IDF%DY)
     ENDDO
     DO IC=0,MP(IPLOT)%IDF%NCOL
      CALL IGRJOIN(MP(IPLOT)%IDF%XMIN+IC*MP(IPLOT)%IDF%DX,MP(IPLOT)%IDF%YMIN, &
                   MP(IPLOT)%IDF%XMIN+IC*MP(IPLOT)%IDF%DX,MP(IPLOT)%IDF%YMAX)
     ENDDO

    ELSE

     DO IR=0,MP(IPLOT)%IDF%NROW
      CALL IGRJOIN(MP(IPLOT)%IDF%XMIN,MP(IPLOT)%IDF%SY(IR),MP(IPLOT)%IDF%XMAX,MP(IPLOT)%IDF%SY(IR))
     ENDDO
     DO IC=0,MP(IPLOT)%IDF%NCOL
      CALL IGRJOIN(MP(IPLOT)%IDF%SX(IC),MP(IPLOT)%IDF%YMIN,MP(IPLOT)%IDF%SX(IC),MP(IPLOT)%IDF%YMAX)
     ENDDO

    ENDIF

    CLOSE(MP(IPLOT)%IDF%IU)
    MP(IPLOT)%IDF%IU=0
    CALL IDFDEALLOCATEX(MP(IPLOT)%IDF)

   ENDIF

  ENDIF

  IF(MP(IPLOT)%IPLOT.EQ.5)MP(IPLOT)%IDFNAME=FNAME
  
 ENDIF
ENDDO

RETURN
END SUBROUTINE IDFPLOT_FEATURES_RASTER

!###======================================================================
SUBROUTINE IDFPLOT_FEATURES_EXTENT()
!###======================================================================
USE WINTERACTER
USE RESOURCE
USE MOD_UTL, ONLY : ITOS,UTL_SETTEXTSIZE
USE MODPLOT,ONLY : MP,MXMPLOT,MPW
USE MOD_IDF, ONLY : IDFREAD,IDFDEALLOCATEX,IDFGETEDGE
USE MOD_MDF, ONLY : READMDF,MDFDEALLOCATE,MDF
IMPLICIT NONE
INTEGER :: I,J,IPLOT,N,IROW,ICOL,IDX,IDY
REAL :: TWIDTH,THEIGHT,X,Y,X1,Y1,X2,Y2,XMIN,XMAX,YMIN,YMAX,FRAC
REAL,PARAMETER :: THICKNESS=1.5
LOGICAL :: LEX
CHARACTER(LEN=256) :: FNAME
CHARACTER(LEN=52) :: STRING

I=0; J=0
IF(WMENUGETSTATE(ID_IDFEXTENT,2).EQ.1)I=1   
IF(WMENUGETSTATE(ID_IDFINDICES,2).EQ.1)J=1

CALL UTL_SETTEXTSIZE(TWIDTH,THEIGHT,THICKNESS*0.01) 
CALL WGRTEXTFONT(FFHELVETICA,WIDTH=TWIDTH,HEIGHT=THEIGHT,ISTYLE=0)
CALL WGRTEXTORIENTATION(ALIGNCENTRE,ANGLE=0.0)

CALL IGRLINEWIDTH(1); CALL IGRFILLPATTERN(OUTLINE)
CALL IGRCOLOURN(WRGB(0,0,0)) 

DO IPLOT=1,MXMPLOT

 IF(MP(IPLOT)%ISEL.AND.(MP(IPLOT)%IPLOT.EQ.1.OR. &
                        MP(IPLOT)%IPLOT.EQ.5))THEN

  !## get idf for mdf file
  LEX=.TRUE.
  IF(MP(IPLOT)%IPLOT.EQ.5)THEN
   FNAME=MP(IPLOT)%IDFNAME
   !## read *.mdf file, only to get selected idf to be plotted
   IF(READMDF(MP(IPLOT)%IDFNAME,N))THEN
    MP(IPLOT)%IDFNAME=MDF(MP(IPLOT)%NLIDF)%FNAME
    CALL MDFDEALLOCATE()
   ENDIF
  ENDIF

  IF(LEX)THEN

   !## reread dimensions ... in case different idf is placed ...
   IF(IDFREAD(MP(IPLOT)%IDF,MP(IPLOT)%IDFNAME,0))THEN

    !## display idf extent
    IF(I.EQ.1)CALL IGRRECTANGLE(MP(IPLOT)%IDF%XMIN,MP(IPLOT)%IDF%YMIN, &
                                MP(IPLOT)%IDF%XMAX,MP(IPLOT)%IDF%YMAX)
    !## display indices
    IF(J.EQ.1)THEN
 
     !## size of coord. fit in plotwindow
     XMIN=MAX(MP(IPLOT)%IDF%XMIN,MPW%XMIN); XMAX=MIN(MP(IPLOT)%IDF%XMAX,MPW%XMAX)
     YMIN=MAX(MP(IPLOT)%IDF%YMIN,MPW%YMIN); YMAX=MIN(MP(IPLOT)%IDF%YMAX,MPW%YMAX)

     IF(MP(IPLOT)%IDF%IEQ.EQ.0)THEN
      !## get the accuracy of the drawing, stepsize idx,idy
      CALL IDFGETACCURACY(IDX,IDY,MP(IPLOT)%IDF%NCOL,MP(IPLOT)%IDF%NROW,XMAX-XMIN,YMAX-YMIN)
     ELSEIF(MP(IPLOT)%IDF%IEQ.EQ.1)THEN
      IDX=1; IDY=1
     ENDIF

     FRAC=3.0 !XMAX-XMIN
     
     DO IROW=1,MP(IPLOT)%IDF%NROW,IDY; DO ICOL=1,MP(IPLOT)%IDF%NCOL,IDX
      CALL IDFGETEDGE(MP(IPLOT)%IDF,IROW,ICOL,X1,Y1,X2,Y2)
      IF(X1.GE.XMIN.AND.X2.LE.XMAX.AND.Y2.GE.YMIN.AND.Y1.LE.YMAX)THEN
       X=X1+(X2-X1)/FRAC; Y=Y2-(Y2-Y1)/FRAC
       STRING='('//TRIM(ITOS(IROW))//'-'//TRIM(ITOS(ICOL))//')'
       CALL WGRTEXTSTRING(X,Y,TRIM(STRING))
      ENDIF
     ENDDO; ENDDO

    
    ENDIF

    CLOSE(MP(IPLOT)%IDF%IU)
    MP(IPLOT)%IDF%IU=0
    CALL IDFDEALLOCATEX(MP(IPLOT)%IDF)

   ENDIF

  ENDIF

  IF(MP(IPLOT)%IPLOT.EQ.5)MP(IPLOT)%IDFNAME=FNAME

 ENDIF
ENDDO

RETURN
END SUBROUTINE IDFPLOT_FEATURES_EXTENT

!###======================================================================
SUBROUTINE IDFPLOTAREA(XMIN,YMIN,XMAX,YMAX,I,LPLOT)
!###======================================================================
USE MODPLOT
IMPLICIT NONE
INTEGER,INTENT(IN) :: I
LOGICAL,INTENT(OUT) :: LPLOT
REAL,INTENT(IN) :: XMIN,YMIN,XMAX,YMAX
INTEGER :: J

!## always draw first
IF(I.EQ.1)THEN

 LPLOT=.TRUE.
 IPOLACT=0
 
ELSE

 !## no to be drawn unless proven otherwise
 LPLOT=.FALSE.

 DO J=1,SIZE(IPOLACT) 
  IF(IPOLACT(J).EQ.1)THEN 
   IF(XMIN.LT.POLAREAXY(J,1).OR. & 
      YMIN.LT.POLAREAXY(J,2).OR. &
      XMAX.GT.POLAREAXY(J,3).OR. &
      YMAX.GT.POLAREAXY(J,4))THEN 
    LPLOT=.TRUE.
    EXIT  
   ENDIF 
  ENDIF 
 ENDDO

ENDIF

!## add drawable area only if it get drawn
IF(LPLOT)THEN
 POLAREAXY(I,1)=XMIN; POLAREAXY(I,2)=YMIN; POLAREAXY(I,3)=XMAX; POLAREAXY(I,4)=YMAX
 IPOLACT(I)=1
ENDIF

END SUBROUTINE IDFPLOTAREA

!###======================================================================
LOGICAL FUNCTION IDFDRAW(IDF,LEG,UNITS,IP,XMIN,YMIN,XMAX,YMAX,THICKNESS,LPLOT,UMIN,UMAX)
!###======================================================================
USE WINTERACTER
USE RESOURCE
USE MODPLOT
USE MOD_UTL, ONLY : UTL_WAITMESSAGE,ITOS,UTL_GETUNIT,UTL_IDFCURDIM,UTL_IDFGETCLASS,IDFPLOT1BITMAP
USE MOD_IDF, ONLY : IDFGETVAL_CHECK,IDFGETICLR,IDFREADPART,IDFDEALLOCATEX
USE MOD_IDF_PAR, ONLY : IDFOBJ
IMPLICIT NONE
TYPE(IDFOBJ),INTENT(INOUT) :: IDF
TYPE(LEGENDOBJ),INTENT(INOUT) :: LEG
INTEGER,INTENT(IN) :: UNITS,THICKNESS
INTEGER,INTENT(IN),DIMENSION(4) :: IP
REAL,INTENT(IN) :: XMIN,YMIN,XMAX,YMAX
LOGICAL,INTENT(IN) :: LPLOT
REAL,INTENT(OUT),OPTIONAL :: UMIN,UMAX
TYPE(WIN_MESSAGE) :: MESSAGE
INTEGER :: NC1,NC2,NR1,NR2,ANROW,ANCOL,IRAT,IRAT1,IDY,IDX,I,IOS,&
           J,IWID,IHGT,JBITMAP,KBITMAP,ITYPE,IROW,ICOL,ICLR,ITNODATA,ITRANS
INTEGER :: IDTYPE,IDHANDLE
CHARACTER(LEN=120) :: WAITTXT
REAL :: AX1,AX2,AY1,AY2,X,DX,DY,X1,X2,Y1,Y2,DMIN,DMAX,OX1,OY1,OX2,OY2
INTEGER,ALLOCATABLE,DIMENSION(:) :: IBMPDATA,KBMPDATA
LOGICAL :: LEX

IDFDRAW=.FALSE.

IDTYPE  =WINFODRAWABLE(DRAWABLETYPE)  ! (1)  TYPE (1=WINDOW 2=BITMAP 3=DIALOG/FIELD 4=METAFILE)
IDHANDLE=WINFODRAWABLE(DRAWABLEID)    ! (2)  HANDLE/IDENTIFIER
OX1     =WINFOGRREAL(GRAPHICSAREAMINX)! (7)  LEFT LIMIT OF MAIN GRAPHICS AREA
OY1     =WINFOGRREAL(GRAPHICSAREAMINY)! (8)  LOWER LIMIT OF MAIN GRAPHICS AREA
OX2     =WINFOGRREAL(GRAPHICSAREAMAXX)! (9)  RIGHT LIMIT OF MAIN GRAPHICS AREA
OY2     =WINFOGRREAL(GRAPHICSAREAMAXY)! (10) UPPER LIMIT OF

!  DrawableDialog (3) Dialog identifier (if type=3)
!  DrawableWidth (4) Width in pixels (type=1-3) or internal metafile units (type=4)
!  DrawableHeight (5) Height in pixels (type=1-3) or internal metafile units (type=4)

CALL WINDOWSELECT(0)
ITNODATA=WMENUGETSTATE(ID_TRANSPARANTNODATAIDF,2)
ITRANS  =WMENUGETSTATE(ID_TRANSPARANTIDF,2)

!## try reading last record:
IF(IDFGETVAL_CHECK(IDF,IDF%NROW,IDF%NCOL,X).NE.0)THEN
 CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error reading last record of idf'//CHAR(13)//TRIM(IDF%FNAME),'Error')
 RETURN
ENDIF

CALL UTL_IDFCURDIM(XMIN,YMIN,XMAX,YMAX,IDF,NC1,NC2,NR1,NR2)

LEX=.FALSE.

IF(IP(1).EQ.1.AND.LPLOT)THEN

 WAITTXT='Bitmap Drawing of '//TRIM(IDF%FNAME)//' ...'
 CALL WINDOWSELECT(0); CALL WINDOWOUTSTATUSBAR(2,'Press Escape to stop!'); CALL WINDOWOUTSTATUSBAR(4,TRIM(WAITTXT)//'0%')

 !## active number of columns/row
 IF(IDF%IEQ.EQ.0)THEN
  ANCOL=NC2-NC1+1
  ANROW=NR2-NR1+1
 ELSE
  DX   =IDF%SX(NC2)-IDF%SX(NC1-1)
  DY   =IDF%SY(NR1-1)-IDF%SY(NR2)
  ANCOL=CEILING(DX/IDF%DX)
  ANROW=CEILING(DY/IDF%DY)
 ENDIF

 IF(IDF%IEQ.EQ.0)THEN
  !## get the accuracy of the drawing, stepsize idx,idy
  CALL IDFGETACCURACY(IDX,IDY,ANCOL,ANROW,XMAX-XMIN,YMAX-YMIN)
 ELSEIF(IDF%IEQ.EQ.1)THEN
  IDX=1; IDY=1
 ENDIF

 X=REAL(ANCOL)/REAL(IDX); IWID=CEILING(X)
 X=REAL(ANROW)/REAL(IDY); IHGT=CEILING(X)

 IF(IWID.LE.0.OR.IHGT.LE.0)RETURN

 !## allocate memory for color-plotting in ibmpdata()
 IF(IDF%IEQ.EQ.0)THEN
  ALLOCATE(IBMPDATA(IWID*IHGT),STAT=IOS)
 ELSEIF(IDF%IEQ.EQ.1)THEN
  IWID=MPW%DIX
  IHGT=MPW%DIY
  IOS =0
 ENDIF

 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot display current IDF.'//CHAR(13)// &
    'It needs and array of '//TRIM(ITOS(IWID))//' x '//TRIM(ITOS(IHGT))//' elements','Error')
  RETURN
 ENDIF

 JBITMAP=0; CALL WBITMAPCREATE(JBITMAP,IWID,IHGT)

 !## adjust min/max values current window-level
 DMIN= 10.0E+10; DMAX=-10.0E+10

 IRAT1=0

 IF(IDF%IEQ.EQ.0)THEN

  I=0; J=0
  DO IROW=NR1,NR2,IDY
   I=I+1
   CALL WMESSAGEPEEK(ITYPE,MESSAGE)
   IF(ITYPE.EQ.KEYDOWN.AND.MESSAGE%VALUE1.EQ.KEYESCAPE)EXIT
   DO ICOL=NC1,NC2,IDX
    J          =J+1
    IBMPDATA(J)=IDFGETICLR(IDF,LEG,UNITS,IROW,ICOL,DMIN,DMAX)
   END DO
   CALL UTL_WAITMESSAGE(IRAT,IRAT1,I,IHGT,WAITTXT)
  END DO
  CALL WBITMAPGETDATA(JBITMAP,IBMPDATA)
  DEALLOCATE(IBMPDATA)
  X  = IDF%XMIN+(NC1-1)*IDF%DX;  AX1=(X-MPW%XMIN)/(MPW%XMAX-MPW%XMIN)
  X  = IDF%XMIN+(NC2   *IDF%DX); AX2=(X-MPW%XMIN)/(MPW%XMAX-MPW%XMIN)
  X  = IDF%YMAX-(NR2   *IDF%DY); AY1=(X-MPW%YMIN)/(MPW%YMAX-MPW%YMIN)
  X  = IDF%YMAX-(NR1-1)*IDF%DY;  AY2=(X-MPW%YMIN)/(MPW%YMAX-MPW%YMIN)

 ELSEIF(IDF%IEQ.EQ.1)THEN

  CALL IGRSELECT(DRAWBITMAP,JBITMAP)
  CALL IGRAREA(0.0,0.0,1.0,1.0)
  CALL IGRUNITS(IDF%SX(NC1-1),IDF%SY(NR2),IDF%SX(NC2),IDF%SY(NR1-1))

  CALL IGRFILLPATTERN(SOLID)
  I=0
  DO IROW=NR1,NR2
   I=I+1
   CALL WMESSAGEPEEK(ITYPE,MESSAGE)
   IF(ITYPE.EQ.KEYDOWN.AND.MESSAGE%VALUE1.EQ.KEYESCAPE)EXIT

   Y2=IDF%SY(IROW-1); Y1=IDF%SY(IROW)

   DO ICOL=NC1,NC2

    X1=IDF%SX(ICOL-1); X2=IDF%SX(ICOL)

    ICLR=IDFGETICLR(IDF,LEG,UNITS,IROW,ICOL,DMIN,DMAX)
    CALL IGRCOLOURN(ICLR)
    CALL IGRRECTANGLE(X1,Y1,X2,Y2)

   ENDDO

   CALL UTL_WAITMESSAGE(IRAT,IRAT1,I,IHGT,WAITTXT)
  ENDDO

  CALL IGRSELECT(IDTYPE,IDHANDLE)
  CALL IGRPLOTMODE(MODECOPY)

  X=IDF%SX(NC1-1); AX1=(X-MPW%XMIN)/(MPW%XMAX-MPW%XMIN)
  X=IDF%SX(NC2);   AX2=(X-MPW%XMIN)/(MPW%XMAX-MPW%XMIN)
  X=IDF%SY(NR2);   AY1=(X-MPW%YMIN)/(MPW%YMAX-MPW%YMIN)
  X=IDF%SY(NR1-1); AY2=(X-MPW%YMIN)/(MPW%YMAX-MPW%YMIN)

 ENDIF

 AX1=OX1+AX1*(OX2-OX1)
 AX2=OX1+AX2*(OX2-OX1)
 AY1=OY1+AY1*(OY2-OY1)
 AY2=OY1+AY2*(OY2-OY1)

 !## set target area to be replaced
 CALL IGRAREA(AX1,AY1,AX2,AY2)

 !## compute new pixel values with transparant nodata values
 IF(ITNODATA.EQ.1)THEN

  CALL IGRSELECT(DRAWBITMAP,MPW%IBITMAP)
  CALL WBITMAPSTRETCHMODE(STRETCHDELETE)
  !## get current window in bitmap (only for area set by igrarea)
  KBITMAP=0
  CALL WBITMAPGET(KBITMAP,2)
  !## size of current images
  IWID=WINFOBITMAP(KBITMAP,BITMAPWIDTH)
  IHGT=WINFOBITMAP(KBITMAP,BITMAPHEIGHT)
  !## resize - for performance
  IWID=IWID/4
  IHGT=IHGT/4
  CALL WBITMAPRESIZE(KBITMAP,IWID,IHGT)
  !## create array
  ALLOCATE(KBMPDATA(IWID*IHGT))
  !## read current bitmap's in memory
  CALL WBITMAPPUTDATA(KBITMAP,KBMPDATA) !## previous image  <--- NEEMT VEEL TIJD!
  CALL WBITMAPDESTROY(KBITMAP)
  !## put new window ...
  CALL WBITMAPPUT(JBITMAP,2,1)
  !## ... destroy bitmap ...
  CALL WBITMAPDESTROY(JBITMAP)
  !## ... and get it again
  JBITMAP=0
  CALL WBITMAPGET(JBITMAP,2)
  !## resize - for performance
  CALL WBITMAPRESIZE(JBITMAP,IWID,IHGT)
  !## create array
  ALLOCATE(IBMPDATA(IWID*IHGT))
  CALL WBITMAPPUTDATA(JBITMAP,IBMPDATA) !## new image <--- NEEMT VEEL TIJD
  CALL IDFCOPYCOLOUR(IWID*IHGT,IBMPDATA,KBMPDATA)
  !## restore memory bitmap after adjustments
  CALL WBITMAPGETDATA(JBITMAP,IBMPDATA)
  !## free memory
  DEALLOCATE(IBMPDATA,KBMPDATA)

 ENDIF

 CALL WBITMAPPLOTMODE(MODECOPY)
 IF(ITRANS.EQ.1)CALL WBITMAPPLOTMODE(MODEAND) !## bitwise and

 CALL WBITMAPSTRETCHMODE(STRETCHAND)
 CALL WBITMAPPUT(JBITMAP,2,1)

 CALL WBITMAPPLOTMODE(MODECOPY)
 CALL WBITMAPDESTROY(JBITMAP)
 LEX=.TRUE.
 
ENDIF

!## contouring/vector/texting
IF(IP(2).NE.0.OR.IP(3).NE.0.OR.IP(4).NE.0)THEN

 !## read part
 IF(.NOT.IDFREADPART(IDF,XMIN,YMIN,XMAX,YMAX))RETURN

 DMIN=10.0E10; DMAX=-10.0E10
 DO IROW=1,IDF%NROW; DO ICOL=1,IDF%NCOL
  IF(IDF%X(ICOL,IROW).NE.IDF%NODATA)THEN
   DMIN=MIN(DMIN,IDF%X(ICOL,IROW))
   DMAX=MAX(DMAX,IDF%X(ICOL,IROW))
  ENDIF
 ENDDO; ENDDO

 CALL IGRPLOTMODE(MODECOPY)
 CALL IDFPLOT1BITMAP()
 CALL IGRLINEWIDTH(THICKNESS)

 IF(IP(2).EQ.1)THEN
  WAITTXT='Contouring current window for: '//TRIM(IDF%FNAME)//' ...'
  CALL WINDOWSELECT(0); CALL WINDOWOUTSTATUSBAR(4,TRIM(WAITTXT)//'0%')
  CALL IDFPLOT_CONTOUR(IDF,LEG,IP,THICKNESS)
 ENDIF

 !## vector
 IF(IP(3).EQ.1)THEN
  WAITTXT='Vectoring current window for: '//TRIM(IDF%FNAME)//' ...'
  CALL WINDOWSELECT(0); CALL WINDOWOUTSTATUSBAR(4,TRIM(WAITTXT)//'0%')
  CALL IDFPLOT_VECTOR(IDF,LEG,UNITS,XMAX,XMIN,YMAX,YMIN,IP)
 ENDIF

 !## texting
 IF(IP(4).EQ.1)THEN
  WAITTXT='Texting current window for: '//TRIM(IDF%FNAME)//' ...'
  CALL WINDOWSELECT(0); CALL WINDOWOUTSTATUSBAR(4,TRIM(WAITTXT)//'0%')
  CALL IDFPLOT_TEXTING(IDF,XMAX,XMIN,YMAX,YMIN,ABS(MAX(1,THICKNESS)))
 ENDIF

 CALL IGRLINEWIDTH(1)
 CALL IDFDEALLOCATEX(IDF)
 LEX=.TRUE.
 
ENDIF

CALL WINDOWSELECT(0); CALL WINDOWOUTSTATUSBAR(2,''); CALL WINDOWOUTSTATUSBAR(4,'')

IF(LEX.AND.SUM(IP).GT.0)THEN
 IF(PRESENT(UMIN))UMIN=DMIN; IF(PRESENT(UMAX))UMAX=DMAX
ENDIF

IDFDRAW=.TRUE.

END FUNCTION IDFDRAW

!###======================================================================
SUBROUTINE IDFPLOT_TEXTING(IDF,XMAX,XMIN,YMAX,YMIN,THICKNESS)
!###======================================================================
USE WINTERACTER
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_UTL, ONLY : UTL_SETTEXTSIZE,ITOS,UTL_REALTOSTRING
USE MOD_IDF, ONLY : IDFGETLOC
IMPLICIT NONE
INTEGER,INTENT(IN) :: THICKNESS
REAL,INTENT(IN) :: XMAX,XMIN,YMAX,YMIN
TYPE(IDFOBJ) :: IDF
INTEGER :: IROW,ICOL,IDX,IDY
REAL :: TWIDTH,THEIGHT,X,Y
CHARACTER(LEN=15) :: STRING

!## get the accuracy of the drawing, stepsize idx,idy
IF(IDF%IEQ.EQ.0)THEN
 CALL IDFGETACCURACY(IDX,IDY,IDF%NCOL,IDF%NROW,XMAX-XMIN,YMAX-YMIN)
ELSEIF(IDF%IEQ.EQ.1)THEN
 IDX=1; IDY=1
ENDIF

CALL UTL_SETTEXTSIZE(TWIDTH,THEIGHT,REAL(THICKNESS)*0.01) 
CALL WGRTEXTFONT(FFHELVETICA,WIDTH=TWIDTH,HEIGHT=THEIGHT,ISTYLE=0)
CALL WGRTEXTORIENTATION(ALIGNCENTRE,ANGLE=0.0)

CALL IGRCOLOURN(WRGB(0,0,0))
DO IROW=1,IDF%NROW,IDY; DO ICOL=1,IDF%NCOL,IDX
 IF(IDF%X(ICOL,IROW).EQ.IDF%NODATA)CYCLE
 CALL IDFGETLOC(IDF,IROW,ICOL,X,Y)
 IF(X.GT.XMIN.AND.X.LT.XMAX.AND.Y.GT.YMIN.AND.Y.LT.YMAX)THEN
  STRING=UTL_REALTOSTRING(IDF%X(ICOL,IROW))
  CALL WGRTEXTSTRING(X,Y,TRIM(STRING))
 ENDIF
ENDDO; ENDDO

END SUBROUTINE IDFPLOT_TEXTING

!###======================================================================
SUBROUTINE IDFPLOT_VECTOR(IDF,LEG,UNITS,XMAX,XMIN,YMAX,YMIN,IP)
!###======================================================================
USE WINTERACTER
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_IDF, ONLY : IDFDEALLOCATEX,IDFGETLOC
USE MODPLOT, ONLY : LEGENDOBJ
USE MOD_IDF, ONLY : IDFREADPART
USE MOD_UTL, ONLY : UTL_IDFGETCLASS
USE MOD_SOF, ONLY : SOF_COMPUTE_GRAD,SOF_COMPUTE_GRAD_3D
IMPLICIT NONE
TYPE(IDFOBJ),INTENT(INOUT) :: IDF
TYPE(LEGENDOBJ),INTENT(INOUT) :: LEG
REAL,INTENT(IN) :: XMAX,XMIN,YMAX,YMIN
INTEGER,INTENT(IN),DIMENSION(3) :: IP
INTEGER,INTENT(IN) :: UNITS
INTEGER :: IROW,ICOL,IDX,IDY,ICLR
REAL :: X,DX,Y,DY,F
REAL :: DZDX,DZDY,DZDZ,A

!## get the accuracy of the drawing, stepsize idx,idy
IF(IDF%IEQ.EQ.0)THEN
 CALL IDFGETACCURACY(IDX,IDY,IDF%NCOL,IDF%NROW,XMAX-XMIN,YMAX-YMIN)
ELSEIF(IDF%IEQ.EQ.1)THEN
 IDX=1; IDY=1
ENDIF

CALL IGRCOLOURN(WRGB(50,50,50))
DO IROW=1,IDF%NROW,IDY; DO ICOL=1,IDF%NCOL,IDX

 IF(IDF%IVF.EQ.0)THEN
  IF(IDF%X(ICOL,IROW).EQ.IDF%NODATA)CYCLE
 ELSEIF(IDF%IVF.EQ.3)THEN
  IF(IDF%XV(ICOL,IROW,1).EQ.IDF%NODATA)CYCLE
 ENDIF

 IF(UNITS.EQ.0)THEN
  !## normal IDF
  IF(IDF%IVF.EQ.0)CALL SOF_COMPUTE_GRAD(IDF,ICOL,IROW,DZDX,DZDY)
!  IF(IDF%IVF.EQ.0)CALL SOF_COMPUTE_GRAD_STEEPEST(IDF,ICOL,IROW,DZDX,DZDY)

  !## 3IDF
  IF(IDF%IVF.EQ.1)CALL SOF_COMPUTE_GRAD_3D(IDF,ICOL,IROW,DZDX,DZDY,DZDZ)

  IF(IP(1).EQ.0.AND.IP(2).EQ.0)THEN
   F=DZDX**2.0+DZDY**2.0; IF(F.NE.0.0)F=SQRT(F)
   IF(F.NE.IDF%NODATA)THEN
    ICLR=UTL_IDFGETCLASS(LEG,F)
   ELSE
    ICLR=WRGB(255,255,255)
   ENDIF
   CALL IGRCOLOURN(ICLR)  
  ENDIF   
  !## radians  
  A=ATAN2(-1.0*DZDY,DZDX)
 ELSE
  A=IDF%X(ICOL,IROW)
 ENDIF
 CALL IDFGETLOC(IDF,IROW,ICOL,X,Y)
 
 DX= 0.5*COS(A)*IDF%DX
 DY= 0.5*SIN(A)*IDF%DY
 CALL IGRJOIN(X,Y,X+DX,Y+DY)
 DX=-0.5*COS(A)*IDF%DX
 DY=-0.5*SIN(A)*IDF%DY
 CALL IGRARROWJOIN(X,Y,X+DX,Y+DY,1)

ENDDO; ENDDO

!  DX= 0.5*COS(IDF%X(ICOL,IROW))*IDF%DX
!  DY= 0.5*SIN(IDF%X(ICOL,IROW))*IDF%DY
!  CALL IGRJOIN(X,Y,X+DX,Y+DY)
!  DX=-0.5*COS(IDF%X(ICOL,IROW))*IDF%DX
!  DY=-0.5*SIN(IDF%X(ICOL,IROW))*IDF%DY
!  CALL IGRARROWJOIN(X,Y,X+DX,Y+DY,1)

END SUBROUTINE IDFPLOT_VECTOR
 
!###======================================================================
SUBROUTINE IDFCOPYCOLOUR(NDIM,IBMPDATA,KBMPDATA)
!###======================================================================
USE WINTERACTER
USE MOD_UTL, ONLY : UTL_GETUNIT
IMPLICIT NONE
INTEGER,INTENT(IN) :: NDIM
INTEGER,INTENT(OUT),DIMENSION(NDIM) :: IBMPDATA
INTEGER,INTENT(INOUT),DIMENSION(NDIM) :: KBMPDATA
INTEGER :: I,IWHITE!,IU

IWHITE=WRGB(255,255,255)
!## only copy rgb-values if not equal to white = rgb-value=wrgb(255,255,255)0
DO I=1,NDIM
 IF(KBMPDATA(I).NE.IWHITE)IBMPDATA(I)=KBMPDATA(I)
END DO

END SUBROUTINE IDFCOPYCOLOUR

!###======================================================================
SUBROUTINE IDFGETACCURACY(NPX,NPY,NCOL,NROW,DX,DY)
!###======================================================================
USE WINTERACTER
USE RESOURCE
USE MODPLOT, ONLY : MPW
USE MOD_UTL, ONLY : ITOS
IMPLICIT NONE
INTEGER,INTENT(IN) :: NCOL,NROW
INTEGER,INTENT(OUT) :: NPX,NPY
REAL,INTENT(IN) :: DX,DY
REAL :: PCX,PCY,ACC

!## pixels neccessary to plot data
PCX=DX*(REAL(MPW%DIX)/(MPW%XMAX-MPW%XMIN))
PCY=DY*(REAL(MPW%DIY)/(MPW%YMAX-MPW%YMIN))
!## step size in x/y direction depending on available pixels
CALL WINDOWSELECT(0)
ACC=1
IF(WMENUGETSTATE(ID_LOWACCURACY,2).EQ.1)       ACC=10
IF(WMENUGETSTATE(ID_MEDIUMACCURACY,2).EQ.1)    ACC=5
IF(WMENUGETSTATE(ID_HIGHACCURACY,2).EQ.1)      ACC=3
IF(WMENUGETSTATE(ID_EXCELLENTACCURACY,2).EQ.1) ACC=1

NPX=MAX(1,INT(REAL(NCOL*ACC)/PCX))
NPY=MAX(1,INT(REAL(NROW*ACC)/PCY))

CALL WINDOWSELECT(0); CALL WINDOWOUTSTATUSBAR(3,'X:'//TRIM(ITOS(NPX))//'/Y:'//TRIM(ITOS(NPY)))

RETURN
END SUBROUTINE
