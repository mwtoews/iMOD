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
MODULE MOD_PLINES_TRACE

USE WINTERACTER
USE RESOURCE
USE OPENGL
USE MOD_COLOURS
USE IMODVAR, ONLY : PI
USE MOD_IDF_PAR
USE IMOD, ONLY : IDFINIT
USE MODPLOT, ONLY : MPW,MP
USE MOD_ASC2IDF_PAR, ONLY : IDFFILE,IGRIDFUNC,CS,NODATA,XYZFNAMES,IXCOL,IYCOL,IZCOL
USE MOD_ASC2IDF, ONLY : ASC2IDF_INT_MAIN
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_UTL, ONLY : ITOS,UTL_GETUNIT,RTOS,JD,UTL_IDATETOJDATE,UTL_WAITMESSAGE,UTL_IDFGETDATE,UTL_CAP, &
           UTL_GETUNIQUE_CHAR,UTL_CREATEDIR,UTL_IDFSNAPTOGRID,UTL_WSELECTFILE
USE MOD_IDF, ONLY : IDFREAD,IDFDEALLOCATEX,IDFALLOCATEX,IDFWRITE,IDFIROWICOL,IDFREADSCALE,IDFGETLOC,IDFCOPY
USE MOD_PLINES_PAR
USE MOD_PLINES_SP, ONLY : TRACEPREPARESP,TRACEREADSP
USE MOD_PLINES_FLOLIN, ONLY : FLOLIN
USE MOD_OSD, ONLY : OSD_OPEN,OSD_IOSTAT_MSG
USE MOD_MANAGER, ONLY : MANAGERDELETE
USE MOD_DEMO_PAR
USE MOD_3D_PAR, ONLY : PLLISTINDEX,PLLISTCLR,PLLISTAGE,STPLISTINDEX,MIDPOS,TOP,BOT,XYZAXES,IDFPLOT,IPATHLINE_3D,VIEWDX,VIEWDY
USE MOD_IPF, ONLY : IPFALLOCATE,IPFDEALLOCATE,IPFREAD2,NIPF,IPFFILE => IPF

REAL,DIMENSION(:),ALLOCATABLE,PRIVATE :: XSP,YSP,ZSP

CONTAINS

 !###======================================================================
 LOGICAL FUNCTION TRACE_3D_INIT(FNAME)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 CHARACTER(LEN=256) :: RUNFILE
 INTEGER :: IBATCH,ILAY,IPLOT,NG,IDEF
 INTEGER,DIMENSION(:),ALLOCATABLE :: ILAYERS
 CHARACTER(LEN=15),DIMENSION(:),ALLOCATABLE :: CLAYERS

 TRACE_3D_INIT=.FALSE.

 IDEF=WMENUGETSTATE(ID_INTERACTIVEPATHLINES_DEF,2)

 IF(TRIM(FNAME).EQ.'')THEN
! IF(.NOT.UTL_WSELECTFILE('iMODPATH runfile (*.run)|*.run|', &
!                  LOADDIALOG+PROMPTON+DIRCHANGE+APPENDEXT, &
!                  RUNFILE,'Select iMODPATH runfile (*.run)'))RETURN
! RUNFILE='d:\iMOD-Gebruikersdag\IMOD_USER\RUNFILES\imodpath.run'
! RUNFILE='d:\IMOD-MODELS\IBRAHYM_V2.0\iMOD\IMOD_USER\RUNFILES\erfverband.run'
! RUNFILE='d:\IMOD-MODELS\COLUMBIA\IMOD_USER\RUNFILES\imodpath.run'
! RUNFILE='d:\IMOD-MODELS\NHI\imodpath.run'
 RUNFILE='d:\IMOD-MODELS\ALBERTA\SYLVAN_LAKE\IMOD_USER\RUNFILES\MODPATH\iMODPATH_IPS.RUN'
! RUNFILE='d:\IMOD-MODELS\vanMarvin\imodpath.run'
!  RUNFILE='d:\IMOD-MODELS\SANFRANSISCO\IMOD_USER\RUNFILES\imodpath.run'
 ELSE
  RUNFILE=FNAME
 ENDIF 
  
 CALL WDIALOGLOAD(ID_D3DSETTINGS_LAYER,ID_D3DSETTINGS_LAYER)
 CALL WDIALOGLOAD(ID_D3DSETTINGS_SINKS,ID_D3DSETTINGS_SINKS)
 CALL WDIALOGLOAD(ID_D3DSETTINGS_LOCATION,ID_D3DSETTINGS_LOCATION)
 CALL WDIALOGLOAD(ID_D3DSETTINGS_IDF,ID_D3DSETTINGS_IDF)
 CALL WDIALOGLOAD(ID_D3DSETTINGS_PART,ID_D3DSETTINGS_PART)
 
 !## deallocate all memory
 CALL TRACEDEALLOCATE(1)

 !## read runfile
 IBATCH=0; IF(.NOT.TRACEREADRUNFILE(RUNFILE,IBATCH))RETURN

 !## take head for determination model-dimensions
 IF(.NOT.IDFREAD(IDF,HFFNAME(1,1,1),0))RETURN
 !## define zoom area for particle tracking - overrule area
 IDF%XMIN=MAX(IDF%XMIN,MPW%XMIN); IDF%XMAX=MIN(IDF%XMAX,MPW%XMAX)
 IDF%YMIN=MAX(IDF%YMIN,MPW%YMIN); IDF%YMAX=MIN(IDF%YMAX,MPW%YMAX)
 CALL UTL_IDFSNAPTOGRID(IDF%XMIN,IDF%XMAX,IDF%YMIN,IDF%YMAX,IDF%DX,IDF%NCOL,IDF%NROW)

 IF(IDF%NCOL.LE.0.OR.IDF%NROW.LE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'The selected results of the iMODPATH runfile are not within'//CHAR(13)// &
      'the limits of the current graphical zoom extent','Error')
  CALL TRACE_3D_CLOSE()
  RETURN
 ENDIF

 !## read data 
 IF(.NOT.TRACECALC_INIT(0))RETURN
 !## read fluxes, default forward simulation
 IF(.NOT.TRACEREADBUDGET(1,0))RETURN 

 !## initiate tracing variables
 PL%IREV =0     !## 0=forward; 1=backward simulation (operates as a switch)
 PL%NPART=0     !## number of particles active
 PL%IPLOTSP=0   !## plotsp
 PL%SPCOLOR=WRGB(200,10,10) !## initial colour of start-locations
 NSPG=0
 
 ALLOCATE(IVISIT(IDF%NCOL*IDF%NROW*NLAY)); IVISIT=INT(0,1)
 ALLOCATE(LVISIT(IDF%NCOL*IDF%NROW*MIN(2,NLAY))); LVISIT=0

 IF(IDEF.EQ.1)THEN
 
  !## fill top/bot to imod manager
  !## select files in the imod manager
  MP%ISEL=.FALSE.
  DO ILAY=1,NLAY
   DO IPLOT=1,SIZE(MP) 
    IF(TRIM(UTL_CAP(MP(IPLOT)%IDFNAME,'U')).EQ.TRIM(UTL_CAP(ITBFNAME(2,ILAY),'U')))MP(IPLOT)%ISEL=.TRUE.
    IF(TRIM(UTL_CAP(MP(IPLOT)%IDFNAME,'U')).EQ.TRIM(UTL_CAP(ITBFNAME(3,ILAY),'U')))MP(IPLOT)%ISEL=.TRUE.
   ENDDO       
  END DO

  !## delete them all from manager
  CALL MANAGERDELETE(IQ=0)

  DO ILAY=1,NLAY
   !## top
   CALL IDFINIT(IDFNAMEGIVEN=ITBFNAME(2,ILAY),LPLOT=.FALSE.,LDEACTIVATE=.FALSE.)
   !## bot
   CALL IDFINIT(IDFNAMEGIVEN=ITBFNAME(3,ILAY),LPLOT=.FALSE.,LDEACTIVATE=.FALSE.)
  ENDDO
 
 ENDIF
 
 ALLOCATE(ILAYERS(NLAY),CLAYERS(NLAY))
 DO ILAY=1,NLAY; CLAYERS(ILAY)='Layer '//TRIM(ITOS(ILAY)); ENDDO; ILAYERS=1
 CALL WDIALOGSELECT(ID_D3DSETTINGS_LAYER)
 CALL WDIALOGPUTMENU(IDF_MENU1,CLAYERS,NLAY,ILAYERS)
 CALL WDIALOGSELECT(ID_D3DSETTINGS_SINKS)
 CALL WDIALOGPUTMENU(IDF_MENU1,CLAYERS,NLAY,ILAYERS)
 DEALLOCATE(ILAYERS,CLAYERS)
 CALL WDIALOGSELECT(ID_D3DSETTINGS_LOCATION)
 CALL WDIALOGSELECT(ID_D3DSETTINGS_IDF)

 CALL WDIALOGSELECT(ID_D3DSETTINGS_PART)
 CALL WDIALOGPUTIMAGE(ID_OPEN,ID_ICONOPEN,1)
 CALL WDIALOGPUTIMAGE(ID_SAVEAS,ID_ICONSAVEAS,1)
 CALL WDIALOGPUTIMAGE(ID_DELETE,ID_ICONDELETE,1)
 NG=WINFOGRID(IDF_GRID1,GRIDROWSMAX)

 ALLOCATE(STPLISTINDEX(NG)); STPLISTINDEX=0

 !## initiate particles
 CALL TRACE_INIT_SP(NG)
 !## maximal number of particles to be traced
 CALL TRACE_AL_SP(1000,NG)

 IF(IDEF.EQ.1)THEN
  DEMO%IDEMO=2; DEMO%CONFLAG=4; DEMO%ACCFLAG=3; DEMO%IFILL=1
 ELSE
  DEMO%IDEMO=0
 ENDIF
 
 CALL WMENUSETSTATE(ID_INTERACTIVEPATHLINES,2,1)
 CALL WMENUSETSTATE(ID_INTERACTIVEPATHLINES,1,0)
 
 TRACE_3D_INIT=.TRUE.
 
 END FUNCTION TRACE_3D_INIT
 
 !###======================================================================
 LOGICAL FUNCTION TRACE_3D_RESET(ICODE) 
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ICODE
 INTEGER :: I,J,N,M
 REAL :: T
 
 TRACE_3D_RESET=.FALSE.
 
 IF(ALLOCATED(PLLISTINDEX))THEN
  DO I=1,SIZE(PLLISTINDEX,1)
   DO J=1,SIZE(PLLISTINDEX,2)
    IF(PLLISTINDEX(I,J).NE.0)THEN
     CALL GLDELETELISTS(PLLISTINDEX(I,J),1_GLSIZEI)
     CALL WINDOWOUTSTATUSBAR(4,'Clearing memory for Pathines '//TRIM(ITOS(I))//'...')
    ENDIF
   ENDDO
  ENDDO
  DEALLOCATE(PLLISTINDEX)
 ENDIF
 IF(ALLOCATED(PLLISTAGE))  DEALLOCATE(PLLISTAGE)
 IF(ALLOCATED(PLLISTCLR))  DEALLOCATE(PLLISTCLR)
  
 CALL WDIALOGSELECT(ID_D3DSETTINGS_TAB8)
 !## length of the particles tail
 CALL WDIALOGGETINTEGER(IDF_INTEGER4,N)
 !## number of groups active
 CALL WDIALOGGETINTEGER(IDF_INTEGER11,M)
 !## no group active
 IF(M.LE.0)THEN
  IF(ICODE.EQ.1)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Create a set of particles first!','Information')
  RETURN
 ENDIF
 
 !## drawinglist index
 ALLOCATE(PLLISTINDEX(N,M)); PLLISTINDEX=0
 !## colour fraction
 ALLOCATE(PLLISTCLR(N)); PLLISTCLR=0
 !## age
 ALLOCATE(PLLISTAGE(N)); PLLISTAGE=0.0
 
 PL%TCUR=0.0
 PL%NPER=0
 PL%NTIME=0
 
 CALL WDIALOGPUTREAL(IDF_REAL7,PL%TCUR) 
 
 TRACE_3D_RESET=.TRUE.
     
 END FUNCTION TRACE_3D_RESET

 !###======================================================================
 SUBROUTINE TRACE_3D_STARTPOINTS_MAIN()
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITYPE,I,ICOL,IROW,IRGB
 
 CALL WDIALOGSELECT(ID_D3DSETTINGS_PART)
 CALL TRACE_3D_STARTPOINTS_GRID()
 
 CALL WDIALOGSHOW(-1,-1,0,3)
 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)
   CASE (FIELDCHANGED)
    SELECT CASE (MESSAGE%VALUE2)
     CASE (IDF_GRID1)
      CALL WGRIDPOS(MESSAGE%Y,ICOL,IROW)
      IF(ICOL.EQ.2)THEN
       IRGB=SP(IROW)%ICLR; CALL WSELECTCOLOUR(IRGB)
       IF(WINFODIALOG(4).EQ.1)THEN
        CALL WGRIDCOLOURCELL(IDF_GRID1,2,IROW,IRGB,IRGB)
        CALL WGRIDPUTCELLINTEGER(IDF_GRID1,2,IROW,IRGB)
       ENDIF
      ENDIF
    END SELECT
   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (ID_OPEN)
      CALL  TRACE_3D_STARTPOINTS_SAVE(MESSAGE%VALUE1)
      CALL TRACE_3D_STARTPOINTS_GRID()
     CASE (ID_SAVEAS)
      CALL  TRACE_3D_STARTPOINTS_SAVE(MESSAGE%VALUE1)
     CASE (IDOK)
      CALL WGRIDGETCHECKBOX(IDF_GRID1,1,SP%IACT ,NSPG)
      CALL WGRIDGETINTEGER (IDF_GRID1,2,SP%ICLR ,NSPG)
      CALL WGRIDGETMENU    (IDF_GRID1,4,SP%IREV ,NSPG); SP%IREV=SP%IREV-1
      CALL WGRIDGETREAL    (IDF_GRID1,5,SP%SPWIDTH,NSPG)
      CALL WGRIDGETREAL    (IDF_GRID1,6,SP%PWIDTH,NSPG)
      EXIT
     CASE (IDCANCEL)
      EXIT
    END SELECT
  END SELECT
 ENDDO
 CALL WDIALOGHIDE(); CALL WDIALOGSELECT(ID_D3DSETTINGS_TAB8)
 
 IF(NSPG.GT.SIZE(SP))THEN
  CALL WDIALOGFIELDSTATE(ID_ADD,0)
 ELSE
  CALL WDIALOGFIELDSTATE(ID_ADD,1)
 ENDIF
 
 END SUBROUTINE TRACE_3D_STARTPOINTS_MAIN
 
 !###======================================================================
 SUBROUTINE TRACE_3D_STARTPOINTS_GRID()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 CALL WGRIDCLEAR(IDF_GRID1)

 IF(NSPG.GT.0)THEN
  CALL WGRIDROWS(IDF_GRID1,NSPG)
  !## fill in dialog
  CALL WGRIDPUTCHECKBOX(IDF_GRID1,1,SP%IACT,NSPG)
  !## number of particles in each group
  CALL WGRIDPUTINTEGER(IDF_GRID1,3,SP%NPART,NSPG)
  CALL WGRIDSTATE(IDF_GRID1,3,2)
  !## assign current colour to group
  CALL WGRIDPUTINTEGER(IDF_GRID1,2,SP%ICLR,NSPG)
  DO I=1,NSPG; CALL WGRIDCOLOURCELL(IDF_GRID1,2,I,SP(I)%ICLR,SP(I)%ICLR); ENDDO
  !## direction of particle
  SP%IREV=SP%IREV+1; CALL WGRIDPUTOPTION(IDF_GRID1,4,SP%IREV,NSPG); SP%IREV=SP%IREV-1
  !## size of the startpoints
  CALL WGRIDPUTREAL(IDF_GRID1,5,SP%SPWIDTH,NSPG)
  !## size of the particles
  CALL WGRIDPUTREAL(IDF_GRID1,6,SP%PWIDTH,NSPG)
 ELSE
  CALL WGRIDROWS(IDF_GRID1,1)
  CALL WDIALOGFIELDSTATE(IDF_GRID1,2)
 ENDIF

 END SUBROUTINE TRACE_3D_STARTPOINTS_GRID
 
 !###======================================================================
 SUBROUTINE TRACE_3D_STARTPOINTS_SAVE(ID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID
 CHARACTER(LEN=256) :: FNAME
 INTEGER :: IU,I,J
 LOGICAL :: LIPF
 
 IU=UTL_GETUNIT()
 
 IF(ID.EQ.ID_OPEN)THEN
  IF(PL%NPART.GT.0)THEN
  
  ENDIF
  IF(.NOT.UTL_WSELECTFILE('iMOD Map (*.ipf)|*.ipf|iMOD Particle File (*.ptf)|*.ptf|',&
           LOADDIALOG+MUSTEXIST+PROMPTON+DIRCHANGE+APPENDEXT,FNAME,'Load iMOD Map (*.ipf,*.ptf)'))RETURN
  CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='READ')
 
  CALL TRACE_3D_STARTPOINTS_RESET()

  READ(IU,'(I10)') NSPG
  DO I=1,MIN(SIZE(SP),NSPG)
   READ(IU,*) SP(I)%NPART,SP(I)%IACT,SP(I)%ICLR,SP(I)%IREV,SP(I)%SPWIDTH,SP(I)%PWIDTH
   ALLOCATE(XSP(SP(I)%NPART),YSP(SP(I)%NPART),ZSP(SP(I)%NPART))
   DO J=1,SP(I)%NPART; READ(IU,'(3F10.2)') XSP(J),YSP(J),ZSP(J); ENDDO
   !## get new location/particle - click from the 3d tool
   CALL TRACE_3D_STARTPOINTS_ASSIGN(I,1,SP(I)%NPART) !XSP,YSP,ZSP)
   DEALLOCATE(XSP,YSP,ZSP)
   !## total particles for this group
   PL%NPART=PL%NPART+SP(I)%NPART
  ENDDO
  CLOSE(IU)
  CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'Succesfully read '//TRIM(ITOS(PL%NPART))//' particles from'//CHAR(13)//TRIM(FNAME),'Information')

 ELSE
  IF(.NOT.UTL_WSELECTFILE('iMOD Map (*.ipf)|*.ipf|iMOD Particle File (*.ptf)|*.ptf|',&
           SAVEDIALOG+DIRCHANGE+APPENDEXT,FNAME,'Save iMOD Map (*.ipf,*.ptf)'))RETURN
  CALL OSD_OPEN(IU,FILE=FNAME,STATUS='UNKNOWN',ACTION='WRITE')
  LIPF=.FALSE.; IF(INDEX(UTL_CAP(FNAME,'U'),'.IPF').GT.0)LIPF=.TRUE.
  IF(LIPF)THEN
   WRITE(IU,'(I10)') NSPG
   WRITE(IU,'(A)') '4'; WRITE(IU,'(A)') 'X'; WRITE(IU,'(A)') 'Y'; WRITE(IU,'(A)') 'Z'; WRITE(IU,'(A)') 'GROUP'; WRITE(IU,'(A)') '0,TXT'
   DO I=1,NSPG
    DO J=1,SP(I)%NPART
     WRITE(IU,'(3(F10.2,1X),I10)') IDF%XMIN+SP(I)%XLC(J),IDF%YMIN+SP(I)%YLC(J),SP(I)%ZLC(J),I
    ENDDO
   ENDDO
  ELSE
   WRITE(IU,'(I10)') NSPG
   DO I=1,NSPG
    WRITE(IU,'(4(I10,1X),2(F10.2,1X))') SP(I)%NPART,SP(I)%IACT,SP(I)%ICLR,SP(I)%IREV,SP(I)%SPWIDTH,SP(I)%PWIDTH
    DO J=1,SP(I)%NPART
     WRITE(IU,'(3(F10.2,1X))') IDF%XMIN+SP(I)%XLC(J),IDF%YMIN+SP(I)%YLC(J),SP(I)%ZLC(J)
    ENDDO
   ENDDO
  ENDIF
  CLOSE(IU)
  CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'Succesfully saved particles to'//CHAR(13)//TRIM(FNAME),'Information')
 ENDIF
 
 END SUBROUTINE TRACE_3D_STARTPOINTS_SAVE
 
 !###======================================================================
 SUBROUTINE TRACE_3D_STARTPOINTS() 
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,II,JJ,K,NX,NZ,N,ISP,IROW,ICOL,ILAY,ISTRONG,IC1,IC2,JLAY,NP, &
    IR1,IR2,ISHAPE,NC,IRANDOM,DR,DC,IIDF,INCLD,ISUBSQ,IL1,IL2,ISGRP,NG,IG,IG1,IG2
 INTEGER,DIMENSION(2) :: IOPT
 REAL :: X,Y,Z,DX,DY,DZ,XC,YC,ZC,Q,QERROR,G2R,OR,R,OFR
 INTEGER,DIMENSION(:),ALLOCATABLE :: ILAYERS,JLAYERS,NSGRP
 TYPE(IDFOBJ) :: PIDF
 LOGICAL :: LIDF,LIPF
 LOGICAL,DIMENSION(2) :: LEX
 
 !## get location of mouse
 CALL WDIALOGSELECT(ID_D3DSETTINGS_TAB8)
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO5,ISP)

 ALLOCATE(NSGRP(NLAY)); NSGRP=0
 ALLOCATE(ILAYERS(NLAY),JLAYERS(NLAY)); ILAYERS=0; JLAYERS=0
 
 SELECT CASE (ISP)
  !## spatial distributed
  CASE (1)

   CALL WDIALOGSELECT(ID_D3DSETTINGS_LOCATION)
   CALL WDIALOGGETREAL(IDF_REAL1,XC)
   CALL WDIALOGGETREAL(IDF_REAL2,YC)
   CALL WDIALOGGETREAL(IDF_REAL3,ZC)
   CALL WDIALOGGETREAL(IDF_REAL11,X)
   CALL WDIALOGGETREAL(IDF_REAL12,Y)
   CALL WDIALOGGETREAL(IDF_REAL13,Z)
   DX=X-XC; DY=Y-YC; DZ=Z-ZC
   CALL WDIALOGGETINTEGER(IDF_INTEGER3,NX)
   CALL WDIALOGGETINTEGER(IDF_INTEGER2,NZ)

   !## allocate memory for temporary storage of locations
   N=NZ*NX**2; ALLOCATE(XSP(N),YSP(N),ZSP(N))
   !## start at lower-left/bottom-corner
   XC=XC-0.5*DX; YC=YC-0.5*DX; ZC=ZC-0.5*DZ
   DX=DX/REAL(NX); DZ=DZ/REAL(NZ)

   N=0; Z=ZC-DZ
   DO K=1,NZ
    Z=Z+DZ; Y=YC-DX
    DO I=1,NX
     Y=Y+DX; X=XC-DX
     DO J=1,NX
      X=X+DX
      N=N+1; XSP(N)=X; YSP(N)=Y; ZSP(N)=Z
     ENDDO
    ENDDO
   ENDDO
   NG=1; NSGRP(NG)=N

  !## spatial distributed via an IDF (2) or an external IPF/IDF-file (5)
  CASE (2,5)
   !## file
   LIDF=.FALSE.; LIPF=.FALSE.
   IF(ISP.EQ.2)THEN; CALL WDIALOGGETMENU(IDF_MENU1,IIDF); LIDF=.TRUE.; ENDIF
   IF(ISP.EQ.5)THEN
    CALL WDIALOGGETSTRING(IDF_STRING2,PIDF%FNAME)
    IF(INDEX(UTL_CAP(PIDF%FNAME,'U'),'.IDF').GT.0)LIDF=.TRUE.
    IF(INDEX(UTL_CAP(PIDF%FNAME,'U'),'.IPF').GT.0)LIDF=.TRUE.
   ENDIF
   
   CALL WDIALOGSELECT(ID_D3DSETTINGS_IDF)
   CALL WDIALOGGETMENU(IDF_MENU1,INCLD)
   CALL WDIALOGGETINTEGER(IDF_INTEGER1,NX)
   CALL WDIALOGGETINTEGER(IDF_INTEGER2,ILAY)
   CALL WDIALOGGETCHECKBOX(IDF_CHECK1,IRANDOM)
   CALL WDIALOGGETREAL(IDF_REAL1,DZ)
   
   IF(LIDF)THEN
    CALL IDFCOPY(IDF,PIDF); IF(.NOT.IDFALLOCATEX(PIDF))THEN; ENDIF
    IF(ISP.EQ.2)THEN
     ILAY=IIDF/2; IF(MOD(IIDF,2).NE.0)ILAY=ILAY+1
     IF(MOD(IIDF,2).EQ.0)THEN; PIDF%X(:,:)=ZBOT(:,:,ILAY)
     ELSE; PIDF%X(:,:)=ZTOP(:,:,ILAY); ENDIF
    ELSEIF(ISP.EQ.5)THEN
     IF(.NOT.IDFREADSCALE(PIDF%FNAME,PIDF,2,0,0.0,0))RETURN
    ENDIF
    DO II=1,2
     N=0
     DO I=1,PIDF%NROW,NX; DO J=1,PIDF%NCOL,NX
      IF(IRANDOM.EQ.0)THEN
       ICOL=J; IROW=I
      ELSE
       CALL RANDOM_NUMBER(X); ICOL=MAX(1.0,PIDF%NCOL*X)
       CALL RANDOM_NUMBER(X); IROW=MAX(1.0,PIDF%NROW*X)
      ENDIF
      IF(PIDF%X(ICOL,IROW).NE.PIDF%NODATA)THEN
       IF(ISP.EQ.2)THEN
        X=QZ(ICOL,IROW,ILAY)
       ELSE
        X=PIDF%X(ICOL,IROW)
       ENDIF
       LEX(1)=.FALSE.
       SELECT CASE (INCLD)
        CASE (1) !## both (-/+)
         LEX(1)=.TRUE.
        CASE (2) !## down (-)
         IF(X.GT.0.0)LEX(1)=.TRUE.
        CASE (3) !## up (+)
         IF(X.LT.0.0)LEX(1)=.TRUE.
       END SELECT
       IF(LEX(1))THEN
        N=N+1
        IF(II.EQ.2)THEN
         CALL IDFGETLOC(PIDF,IROW,ICOL,XSP(N),YSP(N))
         ZSP(N)=PIDF%X(ICOL,IROW)+DZ
        ENDIF
       ENDIF
      ENDIF
     ENDDO; ENDDO
     IF(II.EQ.1)ALLOCATE(XSP(N),YSP(N),ZSP(N))
    ENDDO
    CALL IDFDEALLOCATEX(PIDF)
   ENDIF
   IF(LIPF)THEN
    NIPF=1; CALL IPFALLOCATE(); IPFFILE(1)%FNAME=PIDF%FNAME
    !## x,y,z
    IPFFILE(1)%XCOL=1; IPFFILE(1)%YCOL=2; IPFFILE(1)%ZCOL=3; IPFFILE(1)%Z2COL=4; IPFFILE(1)%QCOL=1
    IF(.NOT.IPFREAD2(1,1,0))RETURN
    N=IPFFILE(1)%NROW; ALLOCATE(XSP(N),YSP(N),ZSP(N))
    DO I=1,N
     XSP(I)=IPFFILE(1)%XYZ(1,I); XSP(I)=IPFFILE(1)%XYZ(2,I); XSP(I)=IPFFILE(1)%XYZ(3,I); XSP(I)=IPFFILE(1)%XYZ(4,I)
    ENDDO
    CALL IPFDEALLOCATE()
   ENDIF
   NG=1; NSGRP(NG)=N

  !## sinks
  CASE (3)
   CALL WDIALOGGETREAL(IDF_REAL10,Q)
   CALL WDIALOGGETMENU(IDF_MENU2,IOPT(1))
   CALL WDIALOGGETCHECKBOX(IDF_CHECK5,ISTRONG)
   
   CALL WDIALOGSELECT(ID_D3DSETTINGS_SINKS)
   CALL WDIALOGGETRADIOBUTTON(IDF_RADIO9,ISHAPE) !## square/circle 
   CALL WDIALOGGETINTEGER(IDF_INTEGER6,NC)       !## number on circle
   CALL WDIALOGGETINTEGER(IDF_INTEGER7,NZ)       !## number vertially
   CALL WDIALOGGETREAL(IDF_REAL14,R)             !## radius
   CALL WDIALOGGETCHECKBOX(IDF_CHECK1,IRANDOM)   !## randomize positions
   CALL WDIALOGGETMENU(IDF_MENU4,INCLD)
   CALL WDIALOGGETMENU(IDF_MENU3,IOPT(2))

   CALL WDIALOGGETMENU(IDF_MENU1,ILAYERS)
   DO I=1,NLAY; IF(ILAYERS(I).EQ.1)ILAYERS(I)=I; ENDDO; JLAYERS=ILAYERS
   CALL WDIALOGGETCHECKBOX(IDF_CHECK2,ISUBSQ)    !## subsequent layers
   CALL WDIALOGGETCHECKBOX(IDF_CHECK3,ISGRP)     !## new group per (group of) layer(s)

   IF(ISUBSQ.EQ.1)CALL TRACE_3D_STARTPOINTS_SUBQ(NLAY,ILAYERS,JLAYERS) 

   NSGRP=0
   
   !## from degrees to radians
   G2R=360.0/(2.0*PI); OR=360.0/REAL(NC); OR=OR/G2R

   DO I=1,2
    N=0; NG=0
    DO JLAY=1,NLAY 
    
     IF(ILAYERS(JLAY).EQ.0.OR.JLAYERS(JLAY).EQ.0)CYCLE
     IL1=ILAYERS(JLAY); IL2=JLAYERS(JLAY)

     DO ILAY=IL1,IL2; DO IROW=1,IDF%NROW; DO ICOL=1,IDF%NCOL
       
      !## correct, proven otherwise
      LEX=.TRUE.

      !## check whether strong-sink
      IF(ISTRONG.EQ.1)THEN
       !## forward simulation
       IF(PL%IREV.EQ.0)THEN
        IF(QX(ICOL,IROW,ILAY).LE.0.0.OR.QX(ICOL+1,IROW,ILAY).GE.0.0.OR. &
           QY(ICOL,IROW,ILAY).GE.0.0.OR.QY(ICOL,IROW+1,ILAY).LE.0.0.OR. &
           QZ(ICOL,IROW,ILAY).GE.0.0.OR.QZ(ICOL,IROW,ILAY+1).LE.0.0)LEX(1)=.FALSE.
       !## backward simulation
       ELSE
        IF(QX(ICOL,IROW,ILAY).GE.0.0.OR.QX(ICOL+1,IROW,ILAY).LE.0.0.OR. &
           QY(ICOL,IROW,ILAY).LE.0.0.OR.QY(ICOL,IROW+1,ILAY).GE.0.0.OR. &
           QZ(ICOL,IROW,ILAY).LE.0.0.OR.QZ(ICOL,IROW,ILAY+1).GE.0.0)LEX(1)=.FALSE.
       ENDIF
      ENDIF
      !## no a strong sink
      IF(.NOT.LEX(1))CYCLE
      
      !## net inflow is negative
      QERROR=QSS(ICOL,IROW,ILAY) 

      !## not correct, proven otherwise
      LEX=.FALSE.
      DO J=1,2
       !## forward simulation
       SELECT CASE (IOPT(J))
        CASE (1) !## =
         IF(QERROR.EQ.Q)LEX(J)=.TRUE.
        CASE (2) !## >
         IF(QERROR.GT.Q)LEX(J)=.TRUE.
        CASE (3) !## <
         IF(QERROR.LT.Q)LEX(J)=.TRUE.
        CASE (4) !## >=
         IF(QERROR.GE.Q)LEX(J)=.TRUE.
        CASE (5) !## <=
         IF(QERROR.LE.Q)LEX(J)=.TRUE.
       END SELECT
      ENDDO
      !## and
      IF(INCLD.EQ.2)LEX(1)=LEX(1).AND.LEX(2)
      !## or
      IF(INCLD.EQ.3)LEX(1)=LEX(1).OR.LEX(2)
          
      !## try next
      IF(.NOT.LEX(1))CYCLE
   
      CALL IDFGETLOC(IDF,IROW,ICOL,XC,YC)
      DZ=ZTOP(ICOL,IROW,ILAY)-ZBOT(ICOL,IROW,ILAY)
      IF(IRANDOM.EQ.0)DZ=DZ/REAL(NZ)
     
      DO II=1,NC  !## number on circle      
       !## get coordinates
       IF(IRANDOM.EQ.0)THEN
        X=XC+R*COS(REAL(II)*OR); Y=YC+R*SIN(REAL(II)*OR)
        Z=ZTOP(ICOL,IROW,ILAY)+(DZ*0.5)
       ENDIF
       DO JJ=1,NZ !## number vertically
        N=N+1
        IF(IRANDOM.EQ.0)THEN
         Z=Z-DZ
        ELSE
         CALL RANDOM_NUMBER(X); OFR=(X*360.0)/G2R; X=XC+R*COS(OFR); Y=YC+R*SIN(OFR)
         CALL RANDOM_NUMBER(Z); Z=Z*DZ;            Z=Z +ZBOT(ICOL,IROW,ILAY)
        ENDIF
        IF(I.EQ.2)THEN; XSP(N)=X; YSP(N)=Y; ZSP(N)=Z; ENDIF
       ENDDO
      ENDDO
    
     ENDDO; ENDDO; ENDDO
     IF(NG.EQ.0)THEN
      NG=NG+1
     ELSE
      NG=NG+ISGRP
     ENDIF
     NSGRP(NG)=NSGRP(NG)+N
    
    ENDDO
    IF(I.EQ.1)ALLOCATE(XSP(N),YSP(N),ZSP(N))
   ENDDO
  
  !## border - layers
  CASE (4)
   CALL WDIALOGGETRADIOBUTTON(IDF_RADIO12,IOPT(1))
   CALL WDIALOGSELECT(ID_D3DSETTINGS_LAYER)
   CALL WDIALOGGETINTEGER(IDF_INTEGER8,NZ)
   CALL WDIALOGGETINTEGER(IDF_INTEGER1,NX)
   CALL WDIALOGGETMENU(IDF_MENU1,ILAYERS)
   DO I=1,NLAY; IF(ILAYERS(I).EQ.1)ILAYERS(I)=I; ENDDO; JLAYERS=ILAYERS
   CALL WDIALOGGETCHECKBOX(IDF_CHECK1,IRANDOM)
   CALL WDIALOGGETCHECKBOX(IDF_CHECK2,ISUBSQ)
   CALL WDIALOGGETCHECKBOX(IDF_CHECK3,ISGRP)

   IF(ISUBSQ.EQ.1)CALL TRACE_3D_STARTPOINTS_SUBQ(NLAY,ILAYERS,JLAYERS) 

   NSGRP=0
   
   !## iopt=1 west
   !## iopt=2 north
   !## iopt=3 east
   !## iopt=4 south
   !## iopt=5 all
   
   IC1=1; IC2=IDF%NCOL; IR1=1; IR2=IDF%NROW; DC=NX; DR=NX
   
   !## east
   IF(IOPT(1).EQ.3)THEN; DC=-NX; IC1=IDF%NCOL; IC2=1; ENDIF
   !## south
   IF(IOPT(1).EQ.4)THEN; DR=-NX; IR1=IDF%NROW; IR2=1; ENDIF

   IF(IOPT(1).EQ.5)THEN
    N=NZ*IC2*IR2
   ELSE
    N=NZ*MAX(IC2,IR2)
   ENDIF
   NX=0; DO I=1,NLAY; IF(JLAYERS(I).GT.0)NX=NX+N; ENDDO; N=NX
   ALLOCATE(XSP(N),YSP(N),ZSP(N))

   N=0; NG=0
   DO ILAY=1,NLAY
    IF(ILAYERS(ILAY).EQ.0.OR.JLAYERS(ILAY).EQ.0)CYCLE
    IL1=ILAYERS(ILAY); IL2=JLAYERS(ILAY)
    SELECT CASE (IOPT(1))
     CASE (1,3) !## west/east
      DO IROW=IR1,IR2,DR; DO ICOL=IC1,IC2,DC
       IF(IBOUND(ICOL,IROW,ILAY).EQ.0)CYCLE
       DZ=ZTOP(ICOL,IROW,IL1)-ZBOT(ICOL,IROW,IL2)
       IF(IRANDOM.EQ.0)DZ=DZ/REAL(NZ)
       Z = ZTOP(ICOL,IROW,IL1)+(0.5*DZ)
       CALL IDFGETLOC(IDF,IROW,ICOL,XC,YC)
       DO J=1,NZ
        N=N+1
        IF(IRANDOM.EQ.0)Z=Z-DZ
        IF(IRANDOM.EQ.1)THEN; CALL RANDOM_NUMBER(Z); Z=ZTOP(ICOL,IROW,IL1)-Z*DZ; ENDIF
        XSP(N)=XC; YSP(N)=YC; ZSP(N)=Z
       ENDDO
       EXIT
      ENDDO; ENDDO
     CASE (2,4) !## north/south
      DO ICOL=IC1,IC2,DC; DO IROW=IR1,IR2,DR
       IF(IBOUND(ICOL,IROW,ILAY).EQ.0)CYCLE
       DZ=ZTOP(ICOL,IROW,IL1)-ZBOT(ICOL,IROW,IL2)
       IF(IRANDOM.EQ.0)DZ=DZ/REAL(NZ)
       Z = ZTOP(ICOL,IROW,IL1)+(0.5*DZ)
       CALL IDFGETLOC(IDF,IROW,ICOL,XC,YC)
       DO J=1,NZ
        N=N+1
        IF(IRANDOM.EQ.0)Z=Z-DZ
        IF(IRANDOM.EQ.1)THEN; CALL RANDOM_NUMBER(Z); Z=ZTOP(ICOL,IROW,IL1)-Z*DZ; ENDIF
        XSP(N)=XC; YSP(N)=YC; ZSP(N)=Z
       ENDDO
       EXIT
      ENDDO; ENDDO
     CASE (5) !## all
      DO I=IC1,IC2,DC; DO J=IR1,IR2,DR
       IF(IRANDOM.EQ.0)THEN
        ICOL=I; IROW=J
       ELSE
        CALL RANDOM_NUMBER(X); ICOL=MAX(1.0,IDF%NCOL*X)
        CALL RANDOM_NUMBER(X); IROW=MAX(1.0,IDF%NROW*X)
       ENDIF
       IF(IBOUND(ICOL,IROW,ILAY).EQ.0)CYCLE
       DZ=ZTOP(ICOL,IROW,IL1)-ZBOT(ICOL,IROW,IL2)
       IF(IRANDOM.EQ.0)DZ=DZ/REAL(NZ)
       Z = ZTOP(ICOL,IROW,IL1)+(0.5*DZ)
       CALL IDFGETLOC(IDF,IROW,ICOL,XC,YC)
       DO II=1,NZ
        N=N+1
        IF(IRANDOM.EQ.0)Z=Z-DZ
        IF(IRANDOM.EQ.1)THEN; CALL RANDOM_NUMBER(Z); Z=ZTOP(ICOL,IROW,IL1)-Z*DZ; ENDIF
        XSP(N)=XC; YSP(N)=YC; ZSP(N)=Z
       ENDDO
      ENDDO; ENDDO
    END SELECT
    IF(NG.EQ.0)THEN
     NG=NG+1
    ELSE
     NG=NG+ISGRP
    ENDIF
    NSGRP(NG)=NSGRP(NG)+N
   ENDDO
  
 END SELECT
 
 !## check if really many particles are supposed to be added
 IF(SUM(NSGRP).GT.10000)THEN
  CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Are you sure to add '//TRIM(ITOS(N))//' particles','Question')
  IF(WINFODIALOG(4).NE.1)N=-1
 ENDIF

 IF(N.GT.0)THEN
  
  IF(NSPG+NG.GT.SIZE(SP))THEN
   CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Total groups '//TRIM(ITOS(NSPG+NG))//' overwrite'//CHAR(13)// &
      'maximal groups sustained ('//TRIM(ITOS(SIZE(SP)))//')'//CHAR(13)//CHAR(13)//'Continue?','Question')
   IF(WINFODIALOG(4).NE.1)NG=0
  ENDIF
  
  DO IG=1,NG

   IG1=1; IF(IG.GT.1)IG1=NSGRP(IG-1)+1; IG2=NSGRP(IG); NP=(IG2-IG1)+1   
   
   NSPG=NSPG+1
   
   IF(NSPG.GT.SIZE(SP))EXIT
   
   !## get new location/particle - click from the 3d tool
   CALL TRACE_3D_STARTPOINTS_ASSIGN(NSPG,IG1,IG2) !,XSP(IG1:IG2),YSP(IG1:IG2),ZSP(IG1:IG2))
   !## total particles for this group
   PL%NPART=PL%NPART+(IG2-IG1+1)
   !## assign current colour to group
   IF(IG.EQ.1)THEN
    SP(NSPG)%ICLR=PL%SPCOLOR
   ELSE
    !## apply other colour for rest of group
    I=MOD(NSPG,MAXCOLOUR); I=MAX(1,I)
    SP(NSPG)%ICLR=ICOLOR(I)
   ENDIF
   !## activate current group
   SP(NSPG)%IACT=1
   !## flow direction current group = current 
   SP(NSPG)%IREV=PL%IREV
   !## plot size
   SP(NSPG)%SPWIDTH=3.0
   SP(NSPG)%PWIDTH=1.0
  
  ENDDO
  
 ELSE
  IF(N.EQ.0)CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'iMOD cannot place particles for current configuration.','Information')
 ENDIF
 
 !## deallocate memory for temporary storage of locations
 IF(ALLOCATED(XSP))    DEALLOCATE(XSP)
 IF(ALLOCATED(YSP))    DEALLOCATE(YSP)
 IF(ALLOCATED(ZSP))    DEALLOCATE(ZSP)
 IF(ALLOCATED(NSGRP))  DEALLOCATE(NSGRP)
 IF(ALLOCATED(ILAYERS))DEALLOCATE(ILAYERS)
 IF(ALLOCATED(JLAYERS))DEALLOCATE(JLAYERS)

 CALL WDIALOGSELECT(ID_D3DSETTINGS_TAB8)
 IF(NSPG.GE.SIZE(SP))CALL WDIALOGFIELDSTATE(ID_ADD,0)
 NSPG=MIN(NSPG,SIZE(SP))
 
 END SUBROUTINE TRACE_3D_STARTPOINTS
 
 !###======================================================================
 SUBROUTINE TRACE_3D_STARTPOINTS_SUBQ(N,ILAYERS,JLAYERS) 
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 INTEGER,INTENT(INOUT),DIMENSION(N) :: ILAYERS,JLAYERS
 INTEGER :: I,J
 
 JLAYERS=0; J=0
 DO I=1,N 
  IF(ILAYERS(I).NE.0)THEN
   IF(J.EQ.0)J=I
  ELSE
   IF(J.NE.0)JLAYERS(J)=I-1; J=0
  ENDIF
 ENDDO
   
 END SUBROUTINE TRACE_3D_STARTPOINTS_SUBQ

 !###======================================================================
 SUBROUTINE TRACE_3D_STARTPOINTS_ASSIGN(IG,IG1,IG2) 
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IG,IG1,IG2
 INTEGER :: IROW,ICOL,ILAY,I,J,K,N
 REAL :: XC,YC,ZC,ZL,DZ

 SP(IG)%NPART=0
 
 DO I=1,IG1,IG2 

  XC=XSP(I); YC=YSP(I); ZC=ZSP(I)
  
  !## location outside model dimensions - skip
  IF(XC.LE.IDF%XMIN.OR.XC.GE.IDF%XMAX.OR. &
     YC.LE.IDF%YMIN.OR.YC.GE.IDF%YMAX)CYCLE

  !## get current irow/icol for model-idf
  CALL IDFIROWICOL(IDF,IROW,ICOL,XC,YC)
 
  !## particle too high - above surfacelevel
  IF(ZC.GT.ZTOP(ICOL,IROW,1))CYCLE
  !## particle too low - below lowest bottom
  IF(ZC.LT.ZBOT(ICOL,IROW,NLAY))CYCLE
    
  !## determine in which layer location is situated, skip positions within clay-layers
  DO ILAY=1,NLAY

   !## skip inactive cells (constant head are strong sinks!)
   IF(IBOUND(ICOL,IROW,ILAY).EQ.0)CYCLE
   
   ZL=0.0

   !## inside current modellayer
   IF(ZC.LE.ZTOP(ICOL,IROW,ILAY).AND.ZC.GE.ZBOT(ICOL,IROW,ILAY))THEN
    !## compute local z: top (zl=1); mid (zl=0.5); bot (zl=0)
    DZ=ZTOP(ICOL,IROW,ILAY)-ZBOT(ICOL,IROW,ILAY)
    IF(DZ.NE.0.0)ZL=(ZC-ZBOT(ICOL,IROW,ILAY))/DZ
   ENDIF
   IF(ZL.EQ.0.0.AND.ILAY.LT.NLAY)THEN
    !## skip inactive/constant head cells under layer
    IF(IBOUND(ICOL,IROW,ILAY+1).EQ.0)CYCLE
    !## inside interbed between modellayers
    IF(ZC.LT.ZBOT(ICOL,IROW,ILAY).AND.ZC.GT.ZTOP(ICOL,IROW,ILAY+1))THEN
     !## compute local z: top (zl=1); mid (zl=0.5); bot (zl=0)
     DZ=ZBOT(ICOL,IROW,ILAY)-ZTOP(ICOL,IROW,ILAY+1)
     IF(DZ.NE.0.0)ZL=(ZTOP(ICOL,IROW,ILAY+1)-ZC)/DZ
    ENDIF
   ENDIF

   IF(ZL.NE.0.0)THEN

    !## count number of particles
    SP(IG)%NPART=SP(IG)%NPART+1
    
    CALL TRACE_3D_STARTPOINTS_MEMORY_SP(IG)  

    SP(IG)%XLC(SP(IG)%NPART)=XC-IDF%XMIN
    SP(IG)%YLC(SP(IG)%NPART)=YC-IDF%YMIN
    SP(IG)%ZLC(SP(IG)%NPART)=ZC
    SP(IG)%ZLL(SP(IG)%NPART)=ZL
    SP(IG)%KLC(SP(IG)%NPART)=ILAY
    SP(IG)%ILC(SP(IG)%NPART)=IROW
    SP(IG)%JLC(SP(IG)%NPART)=ICOL
    SP(IG)%TOT(SP(IG)%NPART)=0.0
    EXIT
    
   ENDIF
  ENDDO

 ENDDO

 END SUBROUTINE TRACE_3D_STARTPOINTS_ASSIGN

 !###======================================================================
 SUBROUTINE TRACE_3D_STARTPOINTS_MEMORY_SP(IG)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IG
 INTEGER :: I,J,N
 
 IF(SP(IG)%NPART.LE.SIZE(SP(IG)%XLC))RETURN
 
 N=SP(IG)%NPART+999
 
 ALLOCATE(SP(IG)%XLC_BU(N),SP(IG)%YLC_BU(N),SP(IG)%ZLC_BU(N), &
          SP(IG)%ZLL_BU(N),SP(IG)%KLC_BU(N),SP(IG)%ILC_BU(N), &
          SP(IG)%JLC_BU(N),SP(IG)%TOT_BU(N))

 DO I=1,SP(IG)%NPART-1
  SP(IG)%XLC_BU(I)=SP(IG)%XLC(I)
  SP(IG)%YLC_BU(I)=SP(IG)%YLC(I)
  SP(IG)%ZLC_BU(I)=SP(IG)%ZLC(I)
  SP(IG)%ZLL_BU(I)=SP(IG)%ZLL(I)
  SP(IG)%KLC_BU(I)=SP(IG)%KLC(I)
  SP(IG)%ILC_BU(I)=SP(IG)%ILC(I)
  SP(IG)%JLC_BU(I)=SP(IG)%JLC(I)
  SP(IG)%TOT_BU(I)=SP(IG)%TOT(I)
 ENDDO

 DEALLOCATE(SP(IG)%XLC,SP(IG)%YLC,SP(IG)%ZLC, &
            SP(IG)%ZLL,SP(IG)%KLC,SP(IG)%ILC, &
            SP(IG)%JLC,SP(IG)%TOT)

 SP(IG)%XLC=>SP(IG)%XLC_BU
 SP(IG)%YLC=>SP(IG)%YLC_BU
 SP(IG)%ZLC=>SP(IG)%ZLC_BU
 SP(IG)%ZLL=>SP(IG)%ZLL_BU
 SP(IG)%KLC=>SP(IG)%KLC_BU
 SP(IG)%ILC=>SP(IG)%ILC_BU
 SP(IG)%JLC=>SP(IG)%JLC_BU
 SP(IG)%TOT=>SP(IG)%TOT_BU
    
 END SUBROUTINE TRACE_3D_STARTPOINTS_MEMORY_SP

 !###======================================================================
 SUBROUTINE TRACE_3D_STARTPOINTS_MEMORY_SPR(IG)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IG
 INTEGER :: I,J,N
 
 IF(SPR(IG)%NPART.LE.SIZE(SPR(IG)%XLC))RETURN
 
 N=SPR(IG)%NPART+999
 
 ALLOCATE(SPR(IG)%XLC_BU(N),SPR(IG)%YLC_BU(N),SPR(IG)%ZLC_BU(N), &
          SPR(IG)%ZLL_BU(N),SPR(IG)%KLC_BU(N),SPR(IG)%ILC_BU(N), &
          SPR(IG)%JLC_BU(N),SPR(IG)%TOT_BU(N))

 !## reset layer
 SPR(IG)%KLC_BU=0
 
 DO I=1,SPR(IG)%NPART-1
  SPR(IG)%XLC_BU(I)=SPR(IG)%XLC(I)
  SPR(IG)%YLC_BU(I)=SPR(IG)%YLC(I)
  SPR(IG)%ZLC_BU(I)=SPR(IG)%ZLC(I)
  SPR(IG)%ZLL_BU(I)=SPR(IG)%ZLL(I)
  SPR(IG)%KLC_BU(I)=SPR(IG)%KLC(I)
  SPR(IG)%ILC_BU(I)=SPR(IG)%ILC(I)
  SPR(IG)%JLC_BU(I)=SPR(IG)%JLC(I)
  SPR(IG)%TOT_BU(I)=SPR(IG)%TOT(I)
 ENDDO

 DEALLOCATE(SPR(IG)%XLC,SPR(IG)%YLC,SPR(IG)%ZLC, &
            SPR(IG)%ZLL,SPR(IG)%KLC,SPR(IG)%ILC, &
            SPR(IG)%JLC,SPR(IG)%TOT)

 SPR(IG)%XLC=>SPR(IG)%XLC_BU
 SPR(IG)%YLC=>SPR(IG)%YLC_BU
 SPR(IG)%ZLC=>SPR(IG)%ZLC_BU
 SPR(IG)%ZLL=>SPR(IG)%ZLL_BU
 SPR(IG)%KLC=>SPR(IG)%KLC_BU
 SPR(IG)%ILC=>SPR(IG)%ILC_BU
 SPR(IG)%JLC=>SPR(IG)%JLC_BU
 SPR(IG)%TOT=>SPR(IG)%TOT_BU
    
 END SUBROUTINE TRACE_3D_STARTPOINTS_MEMORY_SPR
 
 !###======================================================================
 SUBROUTINE TRACE_3D_STARTPOINTS_RESET()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 !## clear all
 DO I=1,SIZE(SP)
  !## open drawing list per timestep/call
  IF(STPLISTINDEX(I).NE.0)CALL GLDELETELISTS(STPLISTINDEX(I),1_GLSIZEI); STPLISTINDEX(I)=0
 ENDDO
        
 NSPG=0; SP%NPART=0; PL%NPART=0
 CALL TRACE_DEAL_SP(); CALL TRACE_DEAL_SPR() 

 !## maximal number of particles to be traced
 CALL TRACE_AL_SP(1000,SIZE(SP))

 CALL WDIALOGSELECT(ID_D3DSETTINGS_TAB8)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER5,PL%NPART)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER11,NSPG)
 CALL WDIALOGFIELDSTATE(ID_ADD,1)
 
 END SUBROUTINE TRACE_3D_STARTPOINTS_RESET
 
 !###======================================================================
 SUBROUTINE TRACE_3D_STARTPOINTS_SHOW()
 !###======================================================================
 IMPLICIT NONE
! REAL :: DX,DY
 REAL(KIND=GLFLOAT) :: X,Y,Z
 INTEGER :: I,IG
  
 VIEWDX=(TOP%X-BOT%X)/2.0_GLFLOAT/XYZAXES(1)
 VIEWDY=(TOP%Y-BOT%Y)/2.0_GLFLOAT/XYZAXES(2)

 !## clear all
 DO IG=1,SIZE(SP)
  !## open drawing list per timestep/call
  IF(STPLISTINDEX(IG).NE.0)CALL GLDELETELISTS(STPLISTINDEX(IG),1_GLSIZEI); STPLISTINDEX(IG)=0
 ENDDO
 
! IF(PL%NPART.LE.0)THEN
!  NSPG=0; SP%NPART=0 
!  CALL WDIALOGSELECT(ID_D3DSETTINGS_TAB8)
!  CALL WDIALOGPUTINTEGER(IDF_INTEGER5,PL%NPART)
!  CALL WDIALOGPUTINTEGER(IDF_INTEGER11,NSPG)
!  RETURN
! ENDIF
 
 !## create seperate list for all of the particles
 DO IG=1,NSPG

  !## list index for,  !## start new drawing list
  STPLISTINDEX(IG)=GLGENLISTS(1); CALL GLNEWLIST(STPLISTINDEX(IG),GL_COMPILE)
 
  CALL GLBEGIN(GL_POINTS)

  DO I=1,SP(IG)%NPART
   !## current position
   X= SP(IG)%XLC(I)+IDF%XMIN; Y=SP(IG)%YLC(I)+IDF%YMIN; Z=SP(IG)%ZLC(I)
   X=(X-MIDPOS%X)/VIEWDX; Y=(Y-MIDPOS%Y)/VIEWDY
   CALL GLVERTEX3F(X,Y,Z)
  ENDDO

  CALL GLEND()
  CALL GLENDLIST()

 ENDDO

 CALL WDIALOGSELECT(ID_D3DSETTINGS_TAB8)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER5,PL%NPART)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER11,NSPG)

 END SUBROUTINE TRACE_3D_STARTPOINTS_SHOW
 
 !###======================================================================
 LOGICAL FUNCTION TRACE_3D_COMPUTE() 
 !###======================================================================
 IMPLICIT NONE
 REAL,PARAMETER :: DYEAR=365.25
 INTEGER :: I,IPART,IDSCH,NPACT,NPER,IG,ITRAPPED,MAXILAY,NTAIL,N,IFREQ,ICAPT,NCAPT
 REAL :: TIME,TTMAX,MAXVELOCITY,XTREP
 LOGICAL :: LEX
 
 TRACE_3D_COMPUTE=.FALSE.

 !## maximal time of simulation - equal to refreshing rate or something to be trickered by the 3d tool
 MAXVELOCITY=0.0

 CALL WDIALOGSELECT(ID_D3DSETTINGS_TAB8)
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO3,PL%ITYPE)

 CALL WDIALOGGETREAL(IDF_REAL4,PL%TMAX); PL%TMAX=PL%TMAX*DYEAR
 CALL WDIALOGGETREAL(IDF_REAL5,PL%TDEL); PL%TDEL=PL%TDEL*DYEAR
 CALL WDIALOGGETREAL(IDF_REAL6,XTREP)
 CALL WDIALOGGETREAL(IDF_REAL7,PL%TCUR); PL%TCUR=PL%TCUR*DYEAR
 CALL WDIALOGGETINTEGER(IDF_INTEGER4,NTAIL)
 CALL WDIALOGGETCHECKBOX(IDF_CHECK7,ITRAPPED)
 CALL WDIALOGGETCHECKBOX(IDF_CHECK8,IFREQ)
 !## make sure ifreq=0 as itrapped=1
 IF(ITRAPPED.EQ.1)IFREQ=0
 CALL WDIALOGGETCHECKBOX(IDF_CHECK6,ICAPT)
 IF(ICAPT.EQ.1)CALL WDIALOGGETMENU(IDF_MENU4,ICAPT)

 CALL WDIALOGGETMENU(IDF_MENU3,ISNK); ISNK=ISNK-1
 CALL WDIALOGGETREAL(IDF_REAL8,FRAC)
 
 PL%NTREP=INT((XTREP+1.0)*REAL(NTAIL))

 !## increase number of timesteps
 PL%NPER =PL%NPER+1
 PL%NTIME=PL%NTIME+1

 !## shift all one position
 DO I=1,SIZE(PLLISTCLR); PLLISTCLR(I)=PLLISTCLR(I)+1; ENDDO
 
 !## restart in stack
 IF(PL%NPER.GT.SIZE(PLLISTINDEX,1))PL%NPER=1

 !## get next timestep
 TTMAX=PL%TCUR+PL%TDEL
 !## maximize it for tmax
 TTMAX=MIN(TTMAX,PL%TMAX)

 PL%TCUR=TTMAX/DYEAR; CALL WDIALOGPUTREAL(IDF_REAL7,PL%TCUR) 

 !## age of current drawing list
 PLLISTAGE(PL%NPER)=TTMAX
 !## position in colouring of current drawing list
 PLLISTCLR(PL%NPER)=1
 
 !## store original direction
 IREV=PL%IREV
 
 !## count number of active particles
 NPACT=0; IMODE=0; IMODE(1)=1
 
 DO IG=1,NSPG

  !## open drawing list per timestep/call
  IF(PLLISTINDEX(PL%NPER,IG).NE.0)CALL GLDELETELISTS(PLLISTINDEX(PL%NPER,IG),1_GLSIZEI)
  
  !## process only whenever particles are active
  IF(SP(IG)%IACT.NE.1)CYCLE
  
  !## decide direction for current particles
  IF(SP(IG)%IREV.NE.IREV)THEN
   CALL TRACEIREV(); IREV=ABS(IREV-1)
  ENDIF

  !## list index for, start new drawing list
  PLLISTINDEX(PL%NPER,IG)=GLGENLISTS(1); CALL GLNEWLIST(PLLISTINDEX(PL%NPER,IG),GL_COMPILE)
 
  !## points
  IF(PL%ITYPE.EQ.2)CALL GLBEGIN(GL_POINTS)

  DO IPART=1,SPR(IG)%NPART 
  
   !## trace selected particle, not yet discharged!
   IF(SPR(IG)%KLC(IPART).GT.0)THEN

    !## time in days
    TIME=SPR(IG)%TOT(IPART)

    !## compute ttmax per particle
    TTMAX=TIME+PL%TDEL
    !## maximize it for tmax
    TTMAX=MIN(TTMAX,PL%TMAX)   

    !# lines
    IF(PL%ITYPE.EQ.1)CALL GLBEGIN(GL_LINE_STRIP)
     
    NPACT=NPACT+1; MAXILAY=0
    CALL FLOLIN(IPART,IMODE(1),TIME,TTMAX,IDSCH, &
                SPR(IG)%JLC(IPART),SPR(IG)%ILC(IPART),SPR(IG)%KLC(IPART),   &
                SPR(IG)%XLC(IPART),SPR(IG)%YLC(IPART),SPR(IG)%ZLC(IPART),SPR(IG)%ZLL(IPART), &
                IBOUND,ZBOT,ZTOP,LDELR,LDELC,QX,QY,QZ,QSS,POR,NCON,IDF%NCOL,IDF%NROW,NLAY,  &
                NLPOR,IDF%NCOL*IDF%NROW*NLAY,NCP1,NRP1,NLP1,ISNK,IREV,FRAC,IMODE(1),   &
                ISS,MAXVELOCITY,DELX,DELY,MAXILAY,IVISIT,LVISIT,NVISIT) 

    !## lines
    IF(PL%ITYPE.EQ.1)CALL GLEND()

    !## clean visited places that have been visited before
    DO I=1,NVISIT; IVISIT(LVISIT(I))=INT(0,1); ENDDO
    !## current time in days
    SPR(IG)%TOT(IPART)=TIME
    !## reached tmax - particle can continue next step
    IF(IDSCH.EQ.7)IDSCH=0
    IF(TIME.GE.PL%TMAX)IDSCH=7
    !## particle discharged whenever idsch.ne.0 or maximal time exceeds
    IF(IDSCH.NE.0)SPR(IG)%KLC(IPART)=-IDSCH 

   ELSE

    !## check whether to restart again
    IF(ITRAPPED.EQ.1)THEN
    
     !## particle stopped - decide whether to restart
     IF(ICAPT.GT.0.AND.SPR(IG)%KLC(IPART).LT.0)THEN
      !## no restart since the exitcode (idsch) is not correct
      IF(ICAPT.NE.ABS(SPR(IG)%KLC(IPART)))CYCLE
     ENDIF

     !## restart particle
     SPR(IG)%ILC(IPART)=SP(IG)%ILC(IPART)
     SPR(IG)%JLC(IPART)=SP(IG)%JLC(IPART)
     SPR(IG)%KLC(IPART)=SP(IG)%KLC(IPART)
     SPR(IG)%XLC(IPART)=SP(IG)%XLC(IPART)
     SPR(IG)%YLC(IPART)=SP(IG)%YLC(IPART)
     SPR(IG)%ZLC(IPART)=SP(IG)%ZLC(IPART)
     SPR(IG)%ZLL(IPART)=SP(IG)%ZLL(IPART)
     SPR(IG)%TOT(IPART)=SP(IG)%TOT(IPART)

    ENDIF
   
   ENDIF
  ENDDO

  !## point strip
  IF(PL%ITYPE.EQ.2)CALL GLEND()
  
  CALL GLENDLIST()
 
  !## create new particles ttmax/pl%trep another integer ...
  IF(IFREQ.EQ.1)THEN
   !## initiate a new series of particles
   IF(MOD(PL%NTIME,PL%NTREP).EQ.0)THEN

    I=0
    !## duplicate
    DO IPART=1,SP(IG)%NPART

     !## particle stopped - decide whether to restart
     IF(ICAPT.GT.0.AND.SPR(IG)%KLC(IPART).LT.0)THEN
      !## still going
      IF(ICAPT.NE.ABS(SPR(IG)%KLC(IPART)))CYCLE
     ENDIF

     !## find "empty" spot of terminated particle
     DO
      I=I+1; IF(I.GT.SPR(IG)%NPART)EXIT
      IF(SPR(IG)%KLC(I).LE.0)EXIT
!      IF(SPR(IG)%KLC(I).EQ.0)EXIT
     ENDDO
     
     IF(I.GT.SPR(IG)%NPART)THEN
      !## count number of particles
      SPR(IG)%NPART=SPR(IG)%NPART+1
      CALL TRACE_3D_STARTPOINTS_MEMORY_SPR(IG)
     ENDIF

     !## add copy of particle 
     SPR(IG)%ILC(I)=SP(IG)%ILC(IPART)
     SPR(IG)%JLC(I)=SP(IG)%JLC(IPART)
     SPR(IG)%KLC(I)=SP(IG)%KLC(IPART)
     SPR(IG)%XLC(I)=SP(IG)%XLC(IPART)
     SPR(IG)%YLC(I)=SP(IG)%YLC(IPART)
     SPR(IG)%ZLC(I)=SP(IG)%ZLC(IPART)
     SPR(IG)%ZLL(I)=SP(IG)%ZLL(IPART)
     SPR(IG)%TOT(I)=SP(IG)%TOT(IPART)
  
     !## add active particles
     NPACT=NPACT+1
    
    ENDDO
   ENDIF
   
  ENDIF
 ENDDO
  
 !## return direction, if neccessary
 IF(PL%IREV.NE.IREV)CALL TRACEIREV()
 
 !## clean drawing list, whenever nothing in it
 IF(NPACT.LE.0)THEN
  DO IG=1,NSPG
   CALL GLDELETELISTS(PLLISTINDEX(PL%NPER,IG),1_GLSIZEI); PLLISTINDEX(PL%NPER,IG)=0
  ENDDO
 ENDIF
 
 CALL WDIALOGPUTINTEGER(IDF_INTEGER6,NPACT)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER7,SUM(SPR%NPART))

 !## trace as long as there are active particles available
 IF(SUM(PLLISTINDEX).NE.0)TRACE_3D_COMPUTE=.TRUE.
 
 END FUNCTION TRACE_3D_COMPUTE

 !###======================================================================
 SUBROUTINE TRACE_3D_COMPUTE_STOP()
 !###======================================================================
 IMPLICIT NONE
 
 IF(PL%IRUN.EQ.0)RETURN
 
 CALL WDIALOGPUTSTRING(ID_START,'Start'); CALL WDIALOGFIELDSTATE(ID_STOP,0)
 CALL WDIALOGFIELDSTATE(IDF_RADIO1,1); CALL WDIALOGFIELDSTATE(IDF_RADIO2,1)
 CALL TRACE_DEAL_SPR(); PL%IRUN=0

 CALL WDIALOGFIELDSTATE(ID_NEW,1); CALL WDIALOGFIELDSTATE(ID_ADD,1)

 CALL WDIALOGPUTINTEGER(IDF_INTEGER6,0)
 CALL WDIALOGPUTINTEGER(IDF_INTEGER7,0)
 CALL WDIALOGPUTREAL(IDF_REAL7,0.0,'(F10.2)')

 END SUBROUTINE TRACE_3D_COMPUTE_STOP
 
 !###======================================================================
 SUBROUTINE TRACE_3D_CLOSE()
 !###======================================================================
 IMPLICIT NONE

 !## deallocate memory
 CALL TRACEDEALLOCATE(1)

 IF(ALLOCATED(IVISIT))DEALLOCATE(IVISIT)
 IF(ALLOCATED(LVISIT))DEALLOCATE(LVISIT)
 
 CALL WDIALOGSELECT(ID_D3DSETTINGS_PART);     CALL WDIALOGUNLOAD()
 CALL WDIALOGSELECT(ID_D3DSETTINGS_LAYER);    CALL WDIALOGUNLOAD()
 CALL WDIALOGSELECT(ID_D3DSETTINGS_LOCATION); CALL WDIALOGUNLOAD()
 CALL WDIALOGSELECT(ID_D3DSETTINGS_SINKS);    CALL WDIALOGUNLOAD()
 
 CALL WINDOWSELECT(0); CALL WMENUSETSTATE(ID_INTERACTIVEPATHLINES,2,0)
 CALL WMENUSETSTATE(ID_INTERACTIVEPATHLINES,1,1)
 
 END SUBROUTINE TRACE_3D_CLOSE
 
 !###======================================================================
 LOGICAL FUNCTION TRACEMAIN(RUNFILE,IBATCH,ICONVERTGEN)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: RUNFILE
 INTEGER,INTENT(IN) :: IBATCH,ICONVERTGEN

 TRACEMAIN=.FALSE.

 !## deallocate all memory
 CALL TRACEDEALLOCATE(1)

 !## read runfile
 IF(TRACEREADRUNFILE(RUNFILE,IBATCH))THEN
  !## trace particles
  IF(TRACE_CALC(IBATCH,ICONVERTGEN))TRACEMAIN=.TRUE.
 ENDIF
  
 END FUNCTION TRACEMAIN
 
 !###======================================================================
 LOGICAL FUNCTION TRACECALC_INIT(IBATCH)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH
 INTEGER :: NCONS
 
 TRACECALC_INIT=.FALSE.

 !## allocate space
 CALL TRACEAL()
 !## create cell-sizes (cell-borders expressed in x,y-coordinates)
 CALL TRACEDELRC()
 !##read information-for particle tracking
 IF(.NOT.TRACEDATIN(NCONS,IBATCH))RETURN
 
 TRACECALC_INIT=.TRUE.
 
 END FUNCTION TRACECALC_INIT
 
 !###======================================================================
 LOGICAL FUNCTION TRACE_CALC(IBATCH,ICONVERTGEN)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH,ICONVERTGEN
 CHARACTER(LEN=50) :: CPERIOD
 CHARACTER(LEN=1000) :: STRING
 REAL :: TIME,TTMAX,DT,MAXVELOCITY
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: I,J,ILAY,ITYPE,IPER,IPART,NPART,NCONS,IPERIOD,NIDSCH,IDSCH,IWIN,IP,IRAT, &
                IRAT1,DPER,MPER,SPER,MAXILAY
 LOGICAL :: LEX
 
 TRACE_CALC=.FALSE.

 IULOG=UTL_GETUNIT()
 IF(IBATCH.EQ.1)THEN
  CALL OSD_OPEN(IULOG,FILE='.\imodpath.log',STATUS='UNKNOWN')
 ELSE
  CALL OSD_OPEN(IULOG,FILE=TRIM(PREFVAL(1))//'\tmp\imodpath.log',STATUS='UNKNOWN')
 ENDIF

 !## take head for determination model-dimensions
 IF(.NOT.IDFREAD(IDF,HFFNAME(1,1,1),0))RETURN; CLOSE(IDF%IU)
 
 IF(.NOT.TRACECALC_INIT(IBATCH))RETURN
 
 ALLOCATE(IVISIT(IDF%NCOL*IDF%NROW*NLAY)); IVISIT=INT(0,1)
 ALLOCATE(LVISIT(IDF%NCOL*IDF%NROW*MIN(2,NLAY))); LVISIT=0

 DO ISPFNAME=1,NSPFNAME
  
  !## read/process particles towards readable format
  IF(.NOT.TRACEPREPARESP(NPART,IBATCH))RETURN
  !## initialize outputfiles
  IF(.NOT.TRACEINITOUTFILES(NPART))RETURN

  !## initialize startpoints memory
  CALL TRACE_INIT_SP(1)
  !## read all particle in memory
  CALL TRACE_AL_SP(NPART,1)
  !## set initial time for each particle to zero
  CALL TRACEREADSP(1,NPART)

  !## copy particles to runtime particles
  CALL TRACE_INIT_SPR(1)
  CALL TRACE_AL_SPR()

  TTMAX  =0.0
  IPERIOD=1
  IRAT   =0
  IRAT1  =IRAT
  MAXVELOCITY=0.0
  IPATHLINE_3D=0
  
  !## transient simulation
  IF(ISS.EQ.1)THEN
   !## forwards
   IF(IREV.EQ.0)THEN
    DO IPER=1,NPER
     IF(PLIPER(IPER,1).LE.JD0.AND.PLIPER(IPER+1,1).GT.JD0)EXIT
    ENDDO   
    TTMAX=PLIPER(IPER,1)-JD0
    WRITE(IULOG,*) 'Initial StartDate Offset:',TTMAX
    IPER=IPER-1; DPER=1; MPER=NPER; SPER=1
   !## backwards
   ELSEIF(IREV.EQ.1)THEN
    DO IPER=NPER,1,-1
     IF(PLIPER(IPER,1).LE.JD0.AND.PLIPER(IPER+1,1).GT.JD0)EXIT
    ENDDO   
    TTMAX=JD0-PLIPER(IPER+1,1)
    WRITE(IULOG,*) 'Initial StartDate Offset:',TTMAX
    IPER=IPER+1; DPER=-1; MPER=1; SPER=NPER
   ENDIF
   
  ENDIF
   
  DO

   !## transient simulation
   IF(ISS.EQ.1)THEN
    IPER=IPER+DPER   
    !## what to do after sequence has almost ended
    IF(IPER.EQ.MPER)THEN 
     LEX=.TRUE.; DT=PLIPER(IPER+1,1)-PLIPER(IPER,1)
     IF(ISTOPCRIT.EQ.3)THEN; TTMAX=TMAX; ELSE; TTMAX=TTMAX+DT; ENDIF
    ELSE
     !## within selected time-window
     LEX=.FALSE.
     !# added one day to pliper(nper+1,1)
     IF(PLIPER(IPER+1,1).LE.JD2+1.AND.PLIPER(IPER,1).GE.JD1)THEN
      !## length of stress-period (days)
      DT =MIN(JD2,PLIPER(IPER+1,1))-MAX(JD1,PLIPER(IPER,1))
      TTMAX=TTMAX+DT
      LEX  =.TRUE.
     ENDIF
    ENDIF

   !## steady-state simulation
   ELSE
    TTMAX=TMAX
    LEX=.TRUE.
    IPER=1
   ENDIF

   !## never exceeds given tmax
   TTMAX=MIN(TTMAX,TMAX)

   IF(LEX)THEN

    CALL WMESSAGEPEEK(ITYPE,MESSAGE)
    IF(ITYPE.EQ.KEYDOWN.AND.MESSAGE%VALUE1.EQ.KEYESCAPE)THEN
     CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Are you sure to terminate current pathline computation?'//CHAR(13)// &
      'Though, current results upto now, are already stored in an IFF-format','Question')
     IF(WINFODIALOG(4).EQ.1)EXIT
    ENDIF

    IF(ISS.EQ.0)WRITE(IULOG,'(1X,A,I10,A,F10.2,A)') 'Using following files for steady-state simulation'
    IF(ISS.EQ.1)WRITE(IULOG,'(1X,A,I10,A,F10.2,A)') 'Using following files for current stressperiod',IPER,' time upto: ',TTMAX,' days'
    DO ILAY=1,NLAY
     STRING=TRIM(ITOS(ILAY))
     J=1; IF(ILAY.EQ.NLAY)J=0
     DO I=1,2+J 
      IP=INDEX(HFFNAME(I,ILAY,IPER),'\',.TRUE.)+1
      STRING=TRIM(STRING)//','//TRIM(HFFNAME(I,ILAY,IPER)(IP:))
     END DO
     WRITE(IULOG,'(A)') TRIM(STRING)
    END DO
    WRITE(IULOG,*)

    !## read time-dependent data for particle tracking
    IF((ISS.EQ.0.AND.ISPFNAME.EQ.1).OR.ISS.EQ.1)THEN
     IF(.NOT.TRACEREADBUDGET(IPER,IBATCH))RETURN 
     !## backwards tracking
     IF(IREV.EQ.1)CALL TRACEIREV()
    ENDIF

    !## start particle loop
    IF(IBATCH.EQ.0)CALL WINDOWSELECT(0)

    IF(ISS.EQ.1)THEN
     CPERIOD=' [Duration '//TRIM(ITOS(INT(DT)))//' day; Period '//TRIM(ITOS(IPERIOD))//', max. '//TRIM(ITOS(INT(TTMAX)))//' days]'
     I=0
     DO IPART=1,SP(1)%NPART
      IF(SP(1)%KLC(IPART).NE.0)I=I+1
     ENDDO
     STRING='Still tracing '//TRIM(ITOS(I))//' out of '//TRIM(ITOS(SP(1)%NPART))//' particles for Stress '// &
                              TRIM(ITOS(IPER))//' out of '//TRIM(ITOS(NPER))//TRIM(CPERIOD)
     IF(IBATCH.EQ.1)WRITE(*,'(A)') TRIM(STRING)
     IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,STRING)
    ELSE
     STRING='Tracing '//TRIM(ITOS(SP(1)%NPART))//' particles (Steady-state) ...'
     IF(IBATCH.EQ.1)WRITE(*,'(A)') TRIM(STRING)
     IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,STRING)
    ENDIF

    NIDSCH=0
    DO IPART=1,SP(1)%NPART
     CALL WMESSAGEPEEK(ITYPE,MESSAGE)
     !## time in days!
     TIME=SPR(1)%TOT(IPART)
     !## trace selected particle, NOT YET discharged!
     IF(SPR(1)%KLC(IPART).NE.0)THEN
      MAXILAY=0
      CALL FLOLIN(IPART,IMODE(1),TIME,TTMAX,IDSCH, &
                  SPR(1)%JLC(IPART),SPR(1)%ILC(IPART),SPR(1)%KLC(IPART),   &
                  SPR(1)%XLC(IPART),SPR(1)%YLC(IPART),SPR(1)%ZLC(IPART),SPR(1)%ZLL(IPART), &
                  IBOUND,ZBOT,ZTOP,LDELR,LDELC,QX,QY,QZ,QSS,POR,NCON,IDF%NCOL,IDF%NROW,NLAY,  &
                  NLPOR,IDF%NCOL*IDF%NROW*NLAY,NCP1,NRP1,NLP1,ISNK,IREV,FRAC,IMODE(1),   &
                  ISS,MAXVELOCITY,DELX,DELY,MAXILAY,IVISIT,LVISIT,NVISIT)
      !## clean visited places that have been visited before
      DO I=1,NVISIT; IVISIT(LVISIT(I))=INT(0,1); ENDDO

      !## time in days
      SPR(1)%TOT(IPART)=TIME !## days
      IF(ISS.EQ.1)THEN
       !## end of current simulation
       IF(IDSCH.EQ.7)IDSCH=0
      ELSE
       !## end of simulation reached!
       IF(SPR(1)%TOT(IPART).GE.TTMAX)IDSCH=7
      ENDIF
      !## particle discharged whenever idsch.ne.0
      IF(IDSCH.NE.0)THEN
       !## write endpoint information current particle that has stopped
       IF(IMODE(2).GT.0)CALL TRACECREATEIPF(IMODE(2),IDSCH,IPART)
       SPR(1)%KLC(IPART)=0
      ELSE
       !## particle NOT discharged!
       NIDSCH=NIDSCH+1
      ENDIF
     ENDIF
     IF(IBATCH.EQ.0)CALL UTL_WAITMESSAGE(IRAT,IRAT1,IPART,SP(1)%NPART,'Busy tracking particle '//TRIM(ITOS(IPART))//' ')
     IF(IBATCH.EQ.1)WRITE(*,'(A,I10)') 'Busy tracking particle ',IPART
    ENDDO
    !## no particles left, stop tracking process!
    IF(NIDSCH.EQ.0)EXIT
   ENDIF

   !## stop while doing a steady-state simulation
   IF(ISS.EQ.0)EXIT 
   !## stop whenever tmax.eq.ttmax
   IF(TTMAX.GE.TMAX)EXIT
   !## determing stopcriterion whenever transient simulation concerned!
   IF(IPER.EQ.MPER)THEN 
    !## stop after last period, assume last period duration is similar to previous one!
    !## continue until particle stops (given tmax, else tmax=10e30)
    IF(ISTOPCRIT.EQ.1.OR.ISTOPCRIT.EQ.3)EXIT
    !## repeat period again tmax
    IF(ISTOPCRIT.EQ.2)THEN
     IPER   =SPER+(-1.0*DPER)
     IPERIOD=IPERIOD+1
    ENDIF
   ENDIF

  ENDDO

  !## write remaining non-stopped particles for endpoints (IDSCH=0)
  IF(IMODE(2).GT.0)THEN
   IDSCH=0
   DO IPART=1,SP(1)%NPART; IF(SPR(1)%KLC(IPART).NE.0)CALL TRACECREATEIPF(IMODE(2),IDSCH,IPART); ENDDO
  ENDIF

  IF(IMODE(1).GT.0)THEN; CLOSE(IMODE(1)); IMODE(1)=1; ENDIF
  IF(IMODE(2).GT.0)THEN; CLOSE(IMODE(2)); IMODE(2)=1; ENDIF
  
  STRING='Completed particle tracking. '// &
   'Results are stored within: '//CHAR(13)//TRIM(IFFFNAME(ISPFNAME))//CHAR(13)//'and added to the iMOD-manager.'//CHAR(13)//CHAR(13)// &
    TRIM(ITOS(SP(1)%NPART))//' particles were released out of '//TRIM(ITOS(TPART))//'. '//CHAR(13)//  &
   'Unreleased particles occured due to inactive/constant head boundary conditions'//CHAR(13)// &
   'and/or particles positioned above/beneath given thresshold.'//CHAR(13)//&
    TRIM(ITOS(NCONS))//' inconsequences were removed from top/bottom information!'//CHAR(13)//CHAR(13)// &
    'IMPORTANT: Maximum velocity that occured: '//TRIM(RTOS(MAXVELOCITY,'E',4))//' m/day'
  WRITE(IULOG,'(/A/)') TRIM(STRING)
  IF(IBATCH.EQ.1)WRITE(*,'(A)') TRIM(STRING)
  IF(IBATCH.EQ.0)THEN
   CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,TRIM(STRING),'Information')
  ENDIF
 
  !## deallocate memory
  CALL TRACEDEALLOCATE(0)
  !## convert to gen if desired
  IF(ICONVERTGEN.EQ.1)CALL TRACECONVERTTOGEN(TRIM(IFFFNAME(ISPFNAME))//'.iff')
  
 ENDDO
 
 DEALLOCATE(IVISIT,LVISIT)
 
 CLOSE(IULOG)

 TRACE_CALC=.TRUE.

 END FUNCTION TRACE_CALC

 !###======================================================================
 SUBROUTINE TRACECONVERTTOGEN(FNAME) 
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 CHARACTER(LEN=256) :: LINE !,FNAME
 INTEGER :: J,IOS,ID
 INTEGER,DIMENSION(3) :: IU
 TYPE IFFOBJ
  INTEGER :: ID,IL,IROW,ICOL
  REAL :: X,Y,Z,T,V
 END TYPE IFFOBJ
 TYPE(IFFOBJ),DIMENSION(2) :: IFF
 LOGICAL :: LEX
 
 !## nothing to do
 IF(IMODE(1).EQ.0)THEN; WRITE(*,'(A)') 'No export to gen since no iff is created'; RETURN; ENDIF
 
 WRITE(*,'(/A/)') 'Busy with converting to GEN format' 
 WRITE(*,'(A)') '- Converting file '//TRIM(FNAME)
 
 INQUIRE(FILE=FNAME,EXIST=LEX)
 IF(.NOT.LEX)THEN; WRITE(*,'(A)') 'Can not find specified gen file'; RETURN; ENDIF

 IU(1)=UTL_GETUNIT(); CALL OSD_OPEN(IU(1),FILE=FNAME                                    ,STATUS='OLD'    ,ACTION='READ' ,IOSTAT=IOS,FORM='FORMATTED')
 IU(2)=UTL_GETUNIT(); CALL OSD_OPEN(IU(2),FILE=FNAME(:INDEX(FNAME,'.',.TRUE.)-1)//'.gen',STATUS='UNKNOWN',ACTION='WRITE',IOSTAT=IOS,FORM='FORMATTED')
 IU(3)=UTL_GETUNIT(); CALL OSD_OPEN(IU(3),FILE=FNAME(:INDEX(FNAME,'.',.TRUE.)-1)//'.dat',STATUS='UNKNOWN',ACTION='WRITE',IOSTAT=IOS,FORM='FORMATTED')

 !## skip header
 DO J=1,10; READ(IU(1),*); ENDDO
 READ(IU(1),*,IOSTAT=IOS) IFF(2)%ID,IFF(2)%IL,IFF(2)%X,IFF(2)%Y,IFF(2)%Z,IFF(2)%T,IFF(2)%V,IFF(2)%IROW,IFF(2)%ICOL
 ID=0
 DO
  READ(IU(1),*,IOSTAT=IOS) IFF(1)%ID,IFF(1)%IL,IFF(1)%X,IFF(1)%Y,IFF(1)%Z,IFF(1)%T,IFF(1)%V,IFF(1)%IROW,IFF(1)%ICOL
  IF(IOS.NE.0)EXIT
  IF(IFF(1)%ID.EQ.IFF(2)%ID)THEN
   ID=ID+1
   WRITE(IU(2),'(I10)') ID 
   WRITE(IU(2),'(2(F10.2,1X))') IFF(2)%X,IFF(2)%Y
   WRITE(IU(2),'(2(F10.2,1X))') IFF(1)%X,IFF(1)%Y
   WRITE(IU(2),'(A)') 'END'
   LINE=TRIM(ITOS(ID))//','//TRIM(ITOS(IFF(1)%IL))//','//TRIM(RTOS(IFF(1)%T,'E',5))
   WRITE(IU(3),'(A)') TRIM(LINE)
  ENDIF
  IFF(2)=IFF(1)
 ENDDO
 
 WRITE(IU(2),'(A)') 'END'

 DO J=1,SIZE(IU); CLOSE(IU(J)); ENDDO
 
 END SUBROUTINE TRACECONVERTTOGEN

 !###======================================================================
 SUBROUTINE TRACEPOSTPROCESSING(IFFFLOW,IPFFLOW,IDFFLOW,IPFFNAME,IPFICOL,ICONVERTGEN) 
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: IFFFLOW,IPFFLOW,IDFFLOW,IPFFNAME
 INTEGER,INTENT(IN),DIMENSION(4) :: IPFICOL
 INTEGER,INTENT(IN) :: ICONVERTGEN
 CHARACTER(LEN=50) :: ERRORMESSAGE
 CHARACTER(LEN=256) :: LINE
 TYPE IFFOBJ
  INTEGER :: ID,IL,IROW,ICOL
  REAL :: X,Y,Z,T,V
 END TYPE IFFOBJ
 TYPE(IFFOBJ),DIMENSION(:),POINTER :: IFF,IFF_BU
 INTEGER :: IU,I,J,N,IOS,IDSCH,IROW,ICOL,IROW1,IROW2,ICOL1,ICOL2,NROW,NCOL
 REAL :: T,D,EPS,DX,DY
 INTEGER,DIMENSION(:,:),ALLOCATABLE :: JU
 INTEGER,DIMENSION(:,:),ALLOCATABLE :: NP
 REAL,DIMENSION(3,2) :: XYZ
 INTEGER,DIMENSION(3) :: ILAY
 INTEGER,DIMENSION(2) :: ID
  
 IF(.NOT.TRACEPOSTPROCESSING_INIT(IPFFNAME,IPFICOL))RETURN
 IMODE=0; IF(IFFFLOW.NE.'')IMODE(1)=1; IF(IPFFLOW.NE.'')IMODE(2)=1
 
 IF(.NOT.IDFREAD(IDF,IDFFLOW,0))RETURN 
 EPS=IDF%DX/10.0
 
 WRITE(*,'(/A/)') 'Busy with postprocessing particles ...'
 IF(IMODE(1).EQ.1)WRITE(*,'(1X,A)') 'Processing '//TRIM(IFFFLOW)
 IF(IMODE(2).EQ.1)WRITE(*,'(1X,A)') 'Processing '//TRIM(IPFFLOW)
 
 !## get ipf-information - equal to inodflow to assign well to locations
 DO I=1,SIZE(IPF)
  DX=IPF(I)%X-IDF%XMIN 
  J =0; IF(MOD(DX,IDF%DX).NE.0.0)J=1
  IPF(I)%ICOL=(DX/IDF%DX)+J
  DY=IDF%YMAX-IPF(I)%Y 
  J =0; IF(MOD(DY,IDF%DY).NE.0.0)J=1
  IPF(I)%IROW=(DY/IDF%DY)+J
!  CALL IDFIROWICOL(IDF,IPF(I)%IROW,IPF(I)%ICOL,IPF(I)%X,IPF(I)%Y)
!  write(*,*) IPF(I)%IROW,IPF(I)%ICOL,IPF(I)%X,IPF(I)%Y
 ENDDO

 !## open all files
 ALLOCATE(JU(NUNQ,2),NP(NUNQ,2)); JU=0
 DO I=1,NUNQ
  IF(IMODE(1).EQ.1)THEN
   JU(I,1)=UTL_GETUNIT()
   J=INDEX(IFFFLOW,'.',.TRUE.)-1; IF(J.LE.0)J=LEN_TRIM(IFFFLOW)
   CALL OSD_OPEN(JU(I,1),FILE=IFFFLOW(:J)//'_'//TRIM(IPF(I)%UNQLABEL)//'.iff',STATUS='REPLACE', &
                         ACTION='WRITE',IOSTAT=IOS,FORM='FORMATTED')
   CALL TRACEINITOUTFILES_IFF(JU(I,1))
  ENDIF
  IF(IMODE(2).EQ.1)THEN
   JU(I,2)=UTL_GETUNIT()
   J=INDEX(IPFFLOW,'.',.TRUE.)-1; IF(J.LE.0)J=LEN_TRIM(IPFFLOW)
   CALL OSD_OPEN(JU(I,2),FILE=IPFFLOW(:J)//'_'//TRIM(IPF(I)%UNQLABEL)//'_.ipf',STATUS='REPLACE', &
                         ACTION='WRITE',IOSTAT=IOS,FORM='FORMATTED')
   CALL TRACEINITOUTFILES_IPF(JU(I,2),0)
  ENDIF
 ENDDO

 NP=0; IU=UTL_GETUNIT()
 !## split pathlines to appropriate files
 IF(IMODE(1).EQ.1)THEN
  IMODE(1)=UTL_GETUNIT()
  CALL OSD_OPEN(IMODE(1),FILE=IFFFLOW,STATUS='OLD',ACTION='READ',IOSTAT=IOS,FORM='FORMATTED')
  DO I=1,10; READ(IMODE(1),*); ENDDO
  ALLOCATE(IFF(100))
  READ(IMODE(1),*,IOSTAT=IOS) IFF(1)%ID,IFF(1)%IL,IFF(1)%X,IFF(1)%Y,IFF(1)%Z,IFF(1)%T,IFF(1)%V,IFF(1)%IROW,IFF(1)%ICOL
  N=1; DO
   N=N+1
   IF(N.GT.SIZE(IFF))THEN
    IF(N.GT.10000)THEN
     WRITE(*,'(/A/)') '### Currently particle '//TRIM(ITOS(IFF(1)%ID))//' probably went wrong, will be skipped ###'
     !## skip particle ... too big to process
     ALLOCATE(IFF(100))
     DO
      READ(IMODE(1),*) IFF(2)%ID,IFF(2)%IL,IFF(2)%X,IFF(2)%Y,IFF(2)%Z,IFF(2)%T,IFF(2)%V
      IF(IFF(2)%ID.NE.IFF(1)%ID)THEN; IFF(1)=IFF(2); N=2; EXIT; ENDIF
     ENDDO
    ELSE
     ALLOCATE(IFF_BU(N+100),STAT=IOS)
     IFF_BU(1:SIZE(IFF))=IFF; DEALLOCATE(IFF); IFF=>IFF_BU
    ENDIF
   ENDIF
   READ(IMODE(1),*,IOSTAT=IOS) IFF(N)%ID,IFF(N)%IL,IFF(N)%X,IFF(N)%Y,IFF(N)%Z,IFF(N)%T,IFF(N)%V,IFF(N)%IROW,IFF(N)%ICOL
   IF(IFF(1)%ID.NE.IFF(N)%ID.OR.IOS.NE.0)THEN

    !## see what ipf gets the data
IFFLOOP: DO I=1,SIZE(IPF)
     IF(IFF(N-1)%IL.EQ.IPF(I)%ILAY)THEN
      IF(IPF(I)%IROW.EQ.IFF(N-1)%IROW.AND.IPF(I)%ICOL.EQ.IFF(N-1)%ICOL)THEN
       NP(IPF(I)%INQ,1)=NP(IPF(I)%INQ,1)+1
       DO J=1,N-1
        LINE=TRIM(ITOS(IFF(J)%ID))  //','//TRIM(ITOS(IFF(J)%IL))//','// &
          TRIM(RTOS(IFF(J)%X,'F',2))//','//TRIM(RTOS(IFF(J)%Y,'F',2))//','//TRIM(RTOS(IFF(J)%Z,'F',3))//','// &
          TRIM(RTOS(IFF(J)%T,'E',5))//','//TRIM(RTOS(IFF(J)%V,'E',5))//','// &
          TRIM(ITOS(IFF(J)%IROW))   //','//TRIM(ITOS(IFF(J)%ICOL))
        WRITE(JU(IPF(I)%INQ,1),'(A)') TRIM(LINE)
       ENDDO
       EXIT IFFLOOP
      ENDIF   
     ENDIF
    ENDDO IFFLOOP
    IF(IOS.NE.0)EXIT
    IFF(1)=IFF(N); N=1
   ENDIF
  ENDDO
  DO I=1,NUNQ; IF(JU(I,1).NE.0)THEN
   IF(NP(I,1).EQ.0)THEN; CLOSE(JU(I,1),STATUS='DELETE'); JU(I,1)=0; ENDIF
   IF(NP(I,1).GT.0)CLOSE(JU(I,1))
  ENDIF; ENDDO
  CLOSE(IMODE(1)); IMODE(1)=1; DEALLOCATE(IFF)
  
 ENDIF
 !## open output channel endpoints
 IF(IMODE(2).EQ.1)THEN
  IMODE(2)=UTL_GETUNIT()
  CALL OSD_OPEN(IMODE(2),FILE=IPFFLOW,STATUS='OLD',ACTION='READ',IOSTAT=IOS,FORM='FORMATTED')
  READ(IMODE(2),*) NROW; READ(IMODE(2),*) NCOL
  DO I=1,NCOL+1; READ(IMODE(2),*); ENDDO
  !## split endpoint to appropriate files
  DO
   READ(IMODE(2),'(A256)',IOSTAT=IOS) LINE
   READ(LINE,*) (XYZ(I,1),I=1,3),ILAY(1),IROW1,ICOL1,(XYZ(I,2),I=1,3),ILAY(2),IROW2,ICOL2,ID(1),T,D,IDSCH,ILAY(3)
   !## see what ipf gets the data
IPFLOOP: DO I=1,SIZE(IPF)
    IF(ILAY(2).EQ.IPF(I)%ILAY)THEN
     IF(IPF(I)%IROW.EQ.IROW2.AND.IPF(I)%ICOL.EQ.ICOL2)THEN
      NP(IPF(I)%INQ,2)=NP(IPF(I)%INQ,2)+1
      WRITE(JU(IPF(I)%INQ,2),'(A)') TRIM(LINE)
      EXIT IPFLOOP
     ENDIF
    ENDIF
   ENDDO IPFLOOP
   IF(IOS.NE.0)EXIT
  ENDDO
  DO I=1,NUNQ; IF(JU(I,2).NE.0)THEN
   IF(NP(I,2).EQ.0)THEN; CLOSE(JU(I,2),STATUS='DELETE'); JU(I,2)=0; ENDIF
   IF(NP(I,2).GT.0)CLOSE(JU(I,2))
  ENDIF; ENDDO
  CLOSE(IMODE(2)); IMODE(2)=1
 ENDIF
 
 ALLOCATE(XYZFNAMES(1)); CS=IDF%DX; NODATA=0.0; IGRIDFUNC=3 !## mean
 IXCOL=1; IYCOL=2; IZCOL=10 !## cumtt
 
 !## construct header
 DO I=1,NUNQ
  WRITE(*,*) I,NP(I,1),NP(I,2)
  IF(IMODE(2).EQ.1)THEN
   IF(NP(I,2).GT.0)THEN
    JU(I,2)=UTL_GETUNIT()
    J=INDEX(IPFFLOW,'.',.TRUE.)-1; IF(J.LE.0)J=LEN_TRIM(IPFFLOW)
    CALL OSD_OPEN(JU(I,2),FILE=IPFFLOW(:J)//'_'//TRIM(IPF(I)%UNQLABEL)//'_.ipf',STATUS='OLD', &
                          ACTION='READ',IOSTAT=IOS,FORM='FORMATTED')
    JU(I,1)=UTL_GETUNIT()
    CALL OSD_OPEN(JU(I,1),FILE=IPFFLOW(:J)//'_'//TRIM(IPF(I)%UNQLABEL)//'.ipf',STATUS='REPLACE', &
                          ACTION='WRITE',IOSTAT=IOS,FORM='FORMATTED')  
    READ(JU(I,2),*); WRITE(JU(I,1),*) NP(I,2)
    DO
     READ(JU(I,2),'(A256)',IOSTAT=IOS) LINE
     IF(IOS.NE.0)EXIT; WRITE(JU(I,1),'(A)') TRIM(LINE)
    ENDDO
    CLOSE(JU(I,2),STATUS='DELETE'); CLOSE(JU(I,1))
    !## rasterize catchment area
    XYZFNAMES(1)=IPFFLOW(:J)//'_'//TRIM(IPF(I)%UNQLABEL)//'.ipf'
    IDFFILE=IPFFLOW(:J)//'_'//TRIM(IPF(I)%UNQLABEL)//'_catchment.idf'

    IF(.NOT.ASC2IDF_INT_MAIN(1,0.0,0.0,0.0,0.0,0))THEN
    ENDIF

   ENDIF
  ENDIF
 ENDDO
 
 !## convert to gen?
 IF(ICONVERTGEN.EQ.1)THEN
  IF(IMODE(1).EQ.1)THEN
   DO I=1,NUNQ
    CALL TRACECONVERTTOGEN(IFFFLOW(:J)//'_'//TRIM(IPF(I)%UNQLABEL)//'.iff')
   ENDDO
  ENDIF
 ENDIF

 END SUBROUTINE TRACEPOSTPROCESSING
 
 !###======================================================================
 LOGICAL FUNCTION TRACEPOSTPROCESSING_INIT(IPFFNAME,IPFICOL)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: IPFFNAME
 INTEGER,INTENT(IN),DIMENSION(4) :: IPFICOL
 CHARACTER(LEN=52),ALLOCATABLE,DIMENSION(:) :: STRING
 INTEGER :: I,J,K,M,IOS,IU,NJ
 
 TRACEPOSTPROCESSING_INIT=.FALSE.
 
 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=IPFFNAME,STATUS='OLD',FORM='FORMATTED',IOSTAT=IOS)
 READ(IU,*) M
 ALLOCATE(IPF(M))
 READ(IU,*) NJ; DO J=1,NJ+1; READ(IU,*); ENDDO
 ALLOCATE(STRING(NJ))
 DO J=1,M
  READ(IU,*) (STRING(K),K=1,NJ)
  READ(STRING(IPFICOL(1)),*) IPF(J)%X;     READ(STRING(IPFICOL(2)),*) IPF(J)%Y
  READ(STRING(IPFICOL(3)),*) IPF(J)%LABEL; READ(STRING(IPFICOL(4)),*) IPF(J)%ILAY
  IPF(J)%LABEL=UTL_CAP(IPF(J)%LABEL,'U')
 ENDDO
 DEALLOCATE(STRING); CLOSE(IU)

 !## get unique items
 IPF%UNQLABEL=IPF%LABEL
 CALL UTL_GETUNIQUE_CHAR(IPF%UNQLABEL,SIZE(IPF),NUNQ)
 IPF%INQ=0
 !## search for unique labels
 DO J=1,SIZE(IPF)
  DO I=1,NUNQ
   IF(IPF(J)%LABEL.EQ.IPF(I)%UNQLABEL)THEN; IPF(J)%INQ=I; EXIT; ENDIF
  ENDDO
 ENDDO
  
 !## found following unique labels
 WRITE(*,'(/A/)') 'Found following unique labels'
 DO I=1,NUNQ
  K=0; DO J=1,SIZE(IPF)
   IF(IPF(J)%INQ.EQ.I)K=K+1
  ENDDO
  WRITE(*,'(A,I10)') TRIM(IPF(I)%UNQLABEL),K
 ENDDO
 
 TRACEPOSTPROCESSING_INIT=.TRUE.
 
 END FUNCTION TRACEPOSTPROCESSING_INIT
 
 !###======================================================================
 LOGICAL FUNCTION TRACEINITOUTFILES(NPART)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NPART
 INTEGER :: IOS,I
 CHARACTER(LEN=50) :: ERRORMESSAGE

 TRACEINITOUTFILES=.FALSE.

 I=INDEX(IFFFNAME(ISPFNAME),'.',.TRUE.)-1
 IF(I.LE.0)I=LEN_TRIM(IFFFNAME(ISPFNAME))
 CALL UTL_CREATEDIR(IFFFNAME(ISPFNAME)(:INDEX(IFFFNAME(ISPFNAME),'\',.TRUE.)-1))
 !## open output channel pathlines
 IF(IMODE(1).GT.0)THEN
  IMODE(1)=UTL_GETUNIT()
  CALL OSD_OPEN(IMODE(1),FILE=IFFFNAME(ISPFNAME)(:I)//'.iff',STATUS='REPLACE',ACTION='WRITE',IOSTAT=IOS,FORM='FORMATTED')
  CALL TRACEINITOUTFILES_IFF(IMODE(1))
 ENDIF
 IF(IMODE(2).GT.0)THEN
  IMODE(2)=UTL_GETUNIT()
  CALL OSD_OPEN(IMODE(2),FILE=IFFFNAME(ISPFNAME)(:I)//'.ipf',STATUS='REPLACE',ACTION='WRITE',IOSTAT=IOS)
  CALL TRACEINITOUTFILES_IPF(IMODE(2),NPART)
 ENDIF
 IF(IOS.NE.0)THEN
  CALL OSD_IOSTAT_MSG(IOS,ERRORMESSAGE)
  IF(IMODE(1).GT.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK, &
                     TRIM(ERRORMESSAGE)//CHAR(13)//TRIM(IFFFNAME(ISPFNAME)),'Error')
  IF(IMODE(2).GT.1)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK, &
                     TRIM(ERRORMESSAGE)//CHAR(13)//TRIM(IFFFNAME(ISPFNAME)),'Error')
  RETURN
 ENDIF

 TRACEINITOUTFILES=.TRUE.

 END FUNCTION TRACEINITOUTFILES

 !###======================================================================
 SUBROUTINE TRACEINITOUTFILES_IFF(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU

 WRITE(IU,'(A)') '9'
 WRITE(IU,'(A)') 'PARTICLE_NUMBER'
 WRITE(IU,'(A)') 'ILAY'
 WRITE(IU,'(A)') 'XCRD.'
 WRITE(IU,'(A)') 'YCRD.'
 WRITE(IU,'(A)') 'ZCRD.'
 WRITE(IU,'(A)') 'TIME(YEARS)'
 WRITE(IU,'(A)') 'VELOCITY(M/DAY)'
 WRITE(IU,'(A)') 'IROW'
 WRITE(IU,'(A)') 'ICOL'

 END SUBROUTINE TRACEINITOUTFILES_IFF

!###======================================================================
 SUBROUTINE TRACEINITOUTFILES_IPF(IU,NPART)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU,NPART

 WRITE(IU,'(I10)') NPART
 WRITE(IU,'(A)') '16'
 WRITE(IU,'(A)') 'SP_XCRD.'
 WRITE(IU,'(A)') 'SP_YCRD.'
 WRITE(IU,'(A)') 'SP_ZCRD.'
 WRITE(IU,'(A)') 'SP_ILAY'
 WRITE(IU,'(A)') 'SP_IROW'
 WRITE(IU,'(A)') 'SP_ICOL'
 WRITE(IU,'(A)') 'EP_XCRD.'
 WRITE(IU,'(A)') 'EP_YCRD.'
 WRITE(IU,'(A)') 'EP_ZCRD.'
 WRITE(IU,'(A)') 'EP_ILAY'
 WRITE(IU,'(A)') 'EP_IROW'
 WRITE(IU,'(A)') 'EP_ICOL'
 WRITE(IU,'(A)') 'TIME(YEARS)'
 WRITE(IU,'(A)') 'DISTANCE'
 WRITE(IU,'(A)') 'IDENT.NO.'
 WRITE(IU,'(A)') 'CAPTURED_BY'
 WRITE(IU,'(A)') '0,TXT'

 END SUBROUTINE TRACEINITOUTFILES_IPF

 !###======================================================================
 LOGICAL FUNCTION TRACEREADRUNFILE(RUNFILE,IBATCH)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH
 CHARACTER(LEN=*),INTENT(IN) :: RUNFILE
 INTEGER :: IU,JU,J,K,IDATE,IPER,ILAY,NJ,IOS,I,M
 LOGICAL :: LEX
 REAL :: X
 
 TRACEREADRUNFILE=.FALSE.

 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=RUNFILE,STATUS='OLD',FORM='FORMATTED',IOSTAT=IOS)
 IF(.NOT.TRACECHECKRUN(IOS,'Can not open runfile:'//TRIM(RUNFILE)))RETURN
 !## optional output types
 READ(IU,*,IOSTAT=IOS) NLAY; IF(.NOT.TRACECHECKRUN(IOS,'NLAY'))RETURN
 IF(IBATCH.EQ.1)WRITE(*,'(A10,I2)') 'NLAY: ',NLAY
 READ(IU,*,IOSTAT=IOS) NPER; IF(.NOT.TRACECHECKRUN(IOS,'NPER'))RETURN
 IF(IBATCH.EQ.1)WRITE(*,'(A10,I2)') 'NPER: ',NPER

 IF(ALLOCATED(HFFNAME)) DEALLOCATE(HFFNAME)
 IF(ALLOCATED(ITBFNAME))DEALLOCATE(ITBFNAME)
 IF(ALLOCATED(SPFNAME)) DEALLOCATE(SPFNAME)
 IF(ALLOCATED(IFFFNAME))DEALLOCATE(IFFFNAME)

 ALLOCATE(HFFNAME(3,NLAY,NPER))  !BDGFRF,BDGFFF,BDGFLF
 ALLOCATE(ITBFNAME(5,NLAY))      !IBOUND,TOP,BOT,POR_AQF,POR_AQT
 HFFNAME=''; ITBFNAME=''
 
 NSPFNAME=1; ALLOCATE(SPFNAME(NSPFNAME),IFFFNAME(NSPFNAME))
 
 READ(IU,'(A256)',IOSTAT=IOS) SPFNAME(1); IF(.NOT.TRACECHECKRUN(IOS,'SPFNAME'))RETURN
 !## try to read sequence number
 READ(SPFNAME(1),*,IOSTAT=IOS) NSPFNAME 
 IF(IOS.EQ.0)THEN
  DEALLOCATE(SPFNAME,IFFFNAME)
  ALLOCATE(SPFNAME(NSPFNAME),IFFFNAME(NSPFNAME))
  DO I=1,NSPFNAME
   READ(IU,*,IOSTAT=IOS) SPFNAME(I)
   IF(.NOT.TRACECHECKRUN(IOS,'SPFNAME('//TRIM(ITOS(I))//')'))RETURN
   IF(IBATCH.EQ.1)WRITE(*,'(A10,A)') 'SPFNAME: '//TRIM(ITOS(I)),TRIM(SPFNAME(I))
   READ(IU,*,IOSTAT=IOS) IFFFNAME(I)
   IF(.NOT.TRACECHECKRUN(IOS,'IFFFNAME('//TRIM(ITOS(I))//')'))RETURN
   IF(IBATCH.EQ.1)WRITE(*,'(A10,A)') 'IFFNAME: '//TRIM(ITOS(I)),TRIM(IFFFNAME(I))
  ENDDO
 ELSE
  NSPFNAME=1; READ(SPFNAME(1),*,IOSTAT=IOS) SPFNAME(1)
  IF(IBATCH.EQ.1)WRITE(*,'(A)') 'SPFNAME: '//TRIM(SPFNAME(1))
  READ(IU,*,IOSTAT=IOS) IFFFNAME(1); IF(.NOT.TRACECHECKRUN(IOS,'IFFFNAME'))RETURN
  IF(IBATCH.EQ.1)WRITE(*,'(A)') 'IFFNAME: '//TRIM(IFFFNAME(1))
 ENDIF
 READ(IU,*,IOSTAT=IOS) (IMODE(I),I=1,2); IF(.NOT.TRACECHECKRUN(IOS,'IMODE(.)'))RETURN
 IF(IBATCH.EQ.1)WRITE(*,'(A10,2I2)') 'IMODE(.): ',IMODE
 READ(IU,*,IOSTAT=IOS) IREV ; IF(.NOT.TRACECHECKRUN(IOS,'IREV'))RETURN
 IF(IBATCH.EQ.1)WRITE(*,'(A10,I2)') 'IREV: ',IREV
 READ(IU,*,IOSTAT=IOS) ISNK;  IF(.NOT.TRACECHECKRUN(IOS,'ISNK'))RETURN
 IF(IBATCH.EQ.1)WRITE(*,'(A10,I2)') 'ISNK: ',ISNK
 ISNK=ISNK-1
 READ(IU,*,IOSTAT=IOS) FRAC;  IF(.NOT.TRACECHECKRUN(IOS,'FRAC'))RETURN
 IF(ISNK.NE.2)FRAC=1.0; FRAC=MIN(1.0,MAX(0.0,FRAC))
 IF(IBATCH.EQ.1)WRITE(*,'(A10,F5.2)') 'FRAC: ',FRAC
 READ(IU,*,IOSTAT=IOS) ISTOPCRIT; IF(.NOT.TRACECHECKRUN(IOS,'ISTOPCRIT'))RETURN
 IF(IBATCH.EQ.1)WRITE(*,'(A10,I2)') 'ISTOPCRIT: ',ISTOPCRIT
 READ(IU,*,IOSTAT=IOS) TMAX;  IF(.NOT.TRACECHECKRUN(IOS,'TMAX'))RETURN
 IF(IBATCH.EQ.1)WRITE(*,'(A10,E10.3)') 'TMAX: ',TMAX
 JD0=0; JD1=0; JD2=0
 READ(IU,*,IOSTAT=IOS) IDATE; IF(.NOT.TRACECHECKRUN(IOS,'START DATE'))RETURN
 IF(IBATCH.EQ.1)WRITE(*,'(A10,I10)') 'START DATE: ',IDATE
 IF(NPER.GT.1)JD0=UTL_IDATETOJDATE(IDATE)
 READ(IU,*,IOSTAT=IOS) IDATE; IF(.NOT.TRACECHECKRUN(IOS,'START WINDOW'))RETURN
 IF(IBATCH.EQ.1)WRITE(*,'(A10,I10)') 'START WINDOW: ',IDATE
 IF(NPER.GT.1)JD1=UTL_IDATETOJDATE(IDATE)
 READ(IU,*,IOSTAT=IOS) IDATE; IF(.NOT.TRACECHECKRUN(IOS,'END WINDOW'))RETURN
 IF(IBATCH.EQ.1)WRITE(*,'(A10,I10)') 'END WINDOW: ',IDATE
 IF(NPER.GT.1)JD2=UTL_IDATETOJDATE(IDATE)

 !## ib,top,bot,por_aqf,por_aqt
 DO ILAY=1,NLAY
  DO J=1,5
   IF(J.NE.5.OR.ILAY.NE.NLAY)THEN
    READ(IU,*,IOSTAT=IOS) ITBFNAME(J,ILAY)
    IF(J.EQ.1)THEN
     IF(.NOT.TRACECHECKRUN(IOS,'IBOUND FOR LAYER '//TRIM(ITOS(ILAY))))RETURN
     IF(IBATCH.EQ.1)WRITE(*,'(A)') 'IBOUND'//TRIM(ITOS(ILAY))//'='//TRIM(ITBFNAME(J,ILAY))
     IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'IBOUND'//TRIM(ITOS(ILAY))//'='//TRIM(ITBFNAME(J,ILAY)))
    ELSEIF(J.EQ.2)THEN
     IF(.NOT.TRACECHECKRUN(IOS,'TOP FOR LAYER '//TRIM(ITOS(ILAY))))RETURN
     IF(IBATCH.EQ.1)WRITE(*,'(A)') 'TOP'//TRIM(ITOS(ILAY))//'='//TRIM(ITBFNAME(J,ILAY))
     IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'TOP'//TRIM(ITOS(ILAY))//'='//TRIM(ITBFNAME(J,ILAY)))
    ELSEIF(J.EQ.3)THEN
     IF(.NOT.TRACECHECKRUN(IOS,'BOT FOR LAYER '//TRIM(ITOS(ILAY))))RETURN
     IF(IBATCH.EQ.1)WRITE(*,'(A)') 'BOT'//TRIM(ITOS(ILAY))//'='//TRIM(ITBFNAME(J,ILAY))
     IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'BOT'//TRIM(ITOS(ILAY))//'='//TRIM(ITBFNAME(J,ILAY)))
    ELSEIF(J.EQ.4)THEN
     IF(.NOT.TRACECHECKRUN(IOS,'POR AQUIFER FOR LAYER '//TRIM(ITOS(ILAY))))RETURN
     IF(IBATCH.EQ.1)WRITE(*,'(A)') 'POR AQUIFER'//TRIM(ITOS(ILAY))//'='//TRIM(ITBFNAME(J,ILAY))
     IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'POR AQUIFER'//TRIM(ITOS(ILAY))//'='//TRIM(ITBFNAME(J,ILAY)))
    ELSEIF(J.EQ.5)THEN
     IF(.NOT.TRACECHECKRUN(IOS,'POR AQUITARD FOR LAYER '//TRIM(ITOS(ILAY))))RETURN
     IF(IBATCH.EQ.1)WRITE(*,'(A)') 'POR AQUITARD'//TRIM(ITOS(ILAY))//'='//TRIM(ITBFNAME(J,ILAY))
     IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'POR AQUITARD'//TRIM(ITOS(ILAY))//'='//TRIM(ITBFNAME(J,ILAY)))
    ENDIF
    READ(ITBFNAME(J,ILAY),*,IOSTAT=IOS) X
    IF(IOS.NE.0)THEN
     INQUIRE(FILE=ITBFNAME(J,ILAY),EXIST=LEX)
     IF(.NOT.LEX)THEN
      IF(.NOT.TRACECHECKRUN(1,CHAR(13)//'File not found: '//TRIM(ITBFNAME(J,ILAY))))RETURN      
      RETURN
     ENDIF
     IOS=0
    ENDIF
    CALL IUPPERCASE(ITBFNAME(J,ILAY))
   ENDIF
  END DO
 END DO
 NJ=3
 ISS=0
 IF(NPER.GT.1)ISS=1
 !## frf,fff,flf
 DO IPER=1,NPER
  DO ILAY=1,NLAY
   DO J=1,NJ
    IF(J.NE.3.OR.ILAY.NE.NLAY)THEN
     READ(IU,*,IOSTAT=IOS) HFFNAME(J,ILAY,IPER)
     IF(J.EQ.1)THEN
      IF(.NOT.TRACECHECKRUN(IOS,'BDGFRF FOR LAYER '//TRIM(ITOS(ILAY))//' IPER '//TRIM(ITOS(IPER))))RETURN
      IF(IBATCH.EQ.1)WRITE(*,'(A)') 'BDGFRF FOR LAYER'//TRIM(ITOS(ILAY))//'='//TRIM(HFFNAME(J,ILAY,IPER))
      IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'BDGFRF FOR LAYER'//TRIM(ITOS(ILAY))//'='//TRIM(HFFNAME(J,ILAY,IPER)))
     ELSEIF(J.EQ.2)THEN
      IF(.NOT.TRACECHECKRUN(IOS,'BDGFFF FOR LAYER '//TRIM(ITOS(ILAY))//' IPER '//TRIM(ITOS(IPER))))RETURN
      IF(IBATCH.EQ.1)WRITE(*,'(A)') 'BDGFFF FOR LAYER'//TRIM(ITOS(ILAY))//'='//TRIM(HFFNAME(J,ILAY,IPER))
      IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'BDGFFF FOR LAYER'//TRIM(ITOS(ILAY))//'='//TRIM(HFFNAME(J,ILAY,IPER)))
     ELSEIF(J.EQ.3)THEN
      IF(.NOT.TRACECHECKRUN(IOS,'BDGFLF FOR LAYER '//TRIM(ITOS(ILAY))//' IPER '//TRIM(ITOS(IPER))))RETURN
      IF(IBATCH.EQ.1)WRITE(*,'(A)') 'BDGFLF FOR LAYER'//TRIM(ITOS(ILAY))//'='//TRIM(HFFNAME(J,ILAY,IPER))
      IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'BDGFLF FOR LAYER'//TRIM(ITOS(ILAY))//'='//TRIM(HFFNAME(J,ILAY,IPER)))
     ENDIF
     INQUIRE(FILE=HFFNAME(J,ILAY,IPER),EXIST=LEX)
     IF(.NOT.LEX)THEN
      IF(.NOT.TRACECHECKRUN(1,'File not found: '//TRIM(HFFNAME(J,ILAY,IPER))))RETURN      
      RETURN
     ENDIF
     CALL IUPPERCASE(HFFNAME(J,ILAY,IPER))
    ENDIF
   END DO
  END DO
 END DO
 CLOSE(IU)

 !## get time data
 IF(ALLOCATED(PLIPER))DEALLOCATE(PLIPER)
 ALLOCATE(PLIPER(NPER+1,1)); PLIPER=0 
 DO IPER=1,NPER
  PLIPER(IPER,1)=UTL_IDATETOJDATE(UTL_IDFGETDATE(HFFNAME(1,1,IPER)))
 ENDDO
 CALL WSORT(PLIPER(:,1),1,NPER)
 !## artifically extent with one day
 PLIPER(NPER+1,1)=PLIPER(NPER,1)+(JD2-PLIPER(NPER,1)) !1
 
 TRACEREADRUNFILE=.TRUE.

 END FUNCTION TRACEREADRUNFILE

 !###======================================================================
 LOGICAL FUNCTION TRACECHECKRUN(IOS,TXT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IOS
 CHARACTER(LEN=*),INTENT(IN) :: TXT

 TRACECHECKRUN=.TRUE.

 IF(IOS.EQ.0)RETURN
 CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error reading parameter in runfile.'//CHAR(13)//TRIM(TXT),'Error')

 TRACECHECKRUN=.FALSE.

 END FUNCTION TRACECHECKRUN

 !###======================================================================
 SUBROUTINE TRACEDEALLOCATE(ICODE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ICODE
 INTEGER :: I
 LOGICAL :: LEX

 CALL TRACE_DEAL_SP();  IF(ALLOCATED(SP)) DEALLOCATE(SP)
 CALL TRACE_DEAL_SPR(); IF(ALLOCATED(SPR))DEALLOCATE(SPR)
  
 IF(ICODE.EQ.0)RETURN

 IF(ALLOCATED(ITBFNAME))DEALLOCATE(ITBFNAME)
 IF(ALLOCATED(HFFNAME)) DEALLOCATE(HFFNAME)
 IF(ALLOCATED(QX))      DEALLOCATE(QX);     IF(ALLOCATED(QY))   DEALLOCATE(QY)
 IF(ALLOCATED(QZ))      DEALLOCATE(QZ);     IF(ALLOCATED(POR))  DEALLOCATE(POR)
 IF(ALLOCATED(DELR))    DEALLOCATE(DELR);   IF(ALLOCATED(DELC)) DEALLOCATE(DELC)
 IF(ALLOCATED(DELX))    DEALLOCATE(DELX);   IF(ALLOCATED(DELY)) DEALLOCATE(DELY)
 IF(ALLOCATED(LDELR))   DEALLOCATE(LDELR);  IF(ALLOCATED(LDELC))DEALLOCATE(LDELC)
 IF(ALLOCATED(ZBOT))    DEALLOCATE(ZBOT);   IF(ALLOCATED(ZTOP)) DEALLOCATE(ZTOP)
 !IF(ALLOCATED(BUFF))    DEALLOCATE(BUFF);   
 IF(ALLOCATED(IBOUND))  DEALLOCATE(IBOUND); IF(ALLOCATED(NCON)) DEALLOCATE(NCON)
 IF(ALLOCATED(PLIPER))  DEALLOCATE(PLIPER); IF(ALLOCATED(QSS))  DEALLOCATE(QSS)
 IF(ALLOCATED(SPFNAME)) DEALLOCATE(SPFNAME)
 IF(ALLOCATED(IFFFNAME))DEALLOCATE(IFFFNAME)
 
 INQUIRE(UNIT=IULOG,OPENED=LEX); IF(LEX)CLOSE(IULOG)

 END SUBROUTINE TRACEDEALLOCATE

 !###======================================================================
 SUBROUTINE TRACECREATEIPF(IU,IDSCH,IPART)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDSCH,IPART,IU
 REAL :: DIST
! INTEGER :: IC1,IC2,IR1,IR2
! CHARACTER(LEN=256) :: LINE
 
 ! WRITE(IU,*) 'SP_XCRD.'
 ! WRITE(IU,*) 'SP_YCRD.'
 ! WRITE(IU,*) 'SP_ZCRD.'
 ! WRITE(IU,*) 'START_ILAY'
 ! WRITE(IU,*) 'START_IROW'
 ! WRITE(IU,*) 'START_ICOL'
 ! WRITE(IU,*) 'EP_XCRD.'
 ! WRITE(IU,*) 'EP_YCRD.'
 ! WRITE(IU,*) 'EP_ZCRD.'
 ! WRITE(IU,*) 'END_ILAY'
 ! WRITE(IU,*) 'END_IROW'
 ! WRITE(IU,*) 'END_ICOL'
 ! WRITE(IU,*) 'IDENT.NO.'
 ! WRITE(IU,*) 'TIME(YEARS)'
 ! WRITE(IU,*) 'DISTANCE'
 ! WRITE(IU,*) 'CAPTURED BY'
 ! WRITE(IU,*) 'MIN_ILAY'
 
! CALL IDFIROWICOL(IDF,IR1,IC1,XLC(IPART,1)+IDF%XMIN,YLC(IPART,1)+IDF%YMIN)
! CALL IDFIROWICOL(IDF,IR2,IC2,XLC(IPART,2)+IDF%XMIN,YLC(IPART,2)+IDF%YMIN)
 
 DIST=SQRT((SP(1)%XLC(IPART)-SPR(1)%XLC(IPART))**2.0+ &
           (SP(1)%YLC(IPART)-SPR(1)%YLC(IPART))**2.0+ &
           (SP(1)%ZLC(IPART)-SPR(1)%ZLC(IPART))**2.0)

! LINE=TRIM(RTOS(SP(1)%XLC(IPART)+IDF%XMIN,'F',2))//','// &
!      TRIM(RTOS(SP(1)%YLC(IPART)+IDF%YMIN,'F',2))//','// &
!      TRIM(RTOS(SP(1)%ZLC(IPART),'F',3))//','// &
!      TRIM(ITOS(SP(1)%KLC(IPART)))//','// &      
!      TRIM(ITOS(SP(1)%JLC(IPART)))//','// &      
!      TRIM(ITOS(SP(1)%ILC(IPART)))//','// &      
!      TRIM(RTOS(SPR(1)%XLC(IPART)+IDF%XMIN,'F',2))//','// &
!      TRIM(RTOS(SPR(1)%YLC(IPART)+IDF%YMIN,'F',2))//','// &
!      TRIM(RTOS(SPR(1)%ZLC(IPART),'F',3))//','// &
!      TRIM(ITOS(SPR(1)%KLC(IPART)))//','// &      
!      TRIM(ITOS(SPR(1)%JLC(IPART)))//','// &      
!      TRIM(ITOS(SPR(1)%ILC(IPART)))//','// &      
!      TRIM(ITOS(IPART))//','// &      
!      TRIM(RTOS(SPR(1)%TOT(IPART)/365.25,'E',5))//','// &
!      TRIM(RTOS(DIST,'F',2))//','// &
!      TRIM(ITOS(IDSCH))//','// &            
!      TRIM(ITOS(MAXILAY(IPART)))
! WRITE(IU,'(A)') TRIM(LINE)

 WRITE(IU,'(2(3(E15.8,1X),3(I10,1X)),2(E15.8,1X),3(I10,1X))') &
       SP(1) %XLC(IPART)+IDF%XMIN, &
       SP(1) %YLC(IPART)+IDF%YMIN, &
       SP(1) %ZLC(IPART),          &
       SP(1) %KLC(IPART),          &
       SP(1) %JLC(IPART),          &
       SP(1) %ILC(IPART),          &
       SPR(1)%XLC(IPART)+IDF%XMIN, &
       SPR(1)%YLC(IPART)+IDF%YMIN, &
       SPR(1)%ZLC(IPART),          &
       SPR(1)%KLC(IPART),          &
       SPR(1)%JLC(IPART),          &
       SPR(1)%ILC(IPART),          &
       SPR(1)%TOT(IPART)/365.25,   &
       DIST,                       &
       IPART,                      &
       IDSCH

 END SUBROUTINE TRACECREATEIPF

 !###======================================================================
 REAL FUNCTION TRACEGETV(XYZT,APOR)
 !###======================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: APOR
 REAL,INTENT(IN),DIMENSION(4,3) :: XYZT
 REAL :: DX,DY,DZ,DT,D

 DX=XYZT(1,2)-XYZT(1,1)
 DY=XYZT(2,2)-XYZT(2,1)
 DZ=XYZT(3,2)-XYZT(3,1)
 DT=XYZT(4,2)-XYZT(4,1)
 D =SQRT(DX**2.0+DY**2.0+DZ**2.0)
 !#m/year
 TRACEGETV=D/DT*APOR

 RETURN
 END FUNCTION

 !###======================================================================
 SUBROUTINE TRACEAL()
 !###======================================================================
 IMPLICIT NONE

 NCP1 =IDF%NCOL+1
 NRP1 =IDF%NROW+1
 NLP1 =NLAY+1
 !## porosity also for aquitards
 NLPOR=NLAY+NLAY-1

 ALLOCATE(QX(NCP1,IDF%NROW,NLAY))
 ALLOCATE(QY(IDF%NCOL,NRP1,NLAY))
 ALLOCATE(QZ(IDF%NCOL,IDF%NROW,NLP1))
 ALLOCATE(POR(IDF%NCOL,IDF%NROW,NLPOR))
 ALLOCATE(DELR(0:IDF%NCOL))
 ALLOCATE(DELC(0:IDF%NROW))
 ALLOCATE(LDELR(IDF%NCOL))
 ALLOCATE(LDELC(IDF%NROW))
 ALLOCATE(DELX(IDF%NCOL))
 ALLOCATE(DELY(IDF%NROW))
 ALLOCATE(ZBOT(IDF%NCOL,IDF%NROW,NLAY))
 ALLOCATE(ZTOP(IDF%NCOL,IDF%NROW,NLAY))
! ALLOCATE(BUFF(IDF%NCOL,IDF%NROW,NLAY))
 ALLOCATE(QSS(IDF%NCOL,IDF%NROW,NLAY))
 ALLOCATE(IBOUND(IDF%NCOL,IDF%NROW,NLAY))
 ALLOCATE(NCON(NLAY))

 RETURN
 END SUBROUTINE

 !###======================================================================
 SUBROUTINE TRACE_INIT_SP(NG)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NG
 INTEGER :: I
 
 !## particle groups
 ALLOCATE(SP(NG)); SP%NPART=0; SP%ICLR=0; SP%IACT=0; SP%IREV=0; SP%SPWIDTH=0.0; SP%PWIDTH=0.0
 
 DO I=1,NG

  NULLIFY(SP(I)%XLC   ,SP(I)%YLC   ,SP(I)%ZLC   ,SP(I)%ZLL   , &
          SP(I)%ILC   ,SP(I)%JLC   ,SP(I)%KLC   ,SP(I)%TOT   )
  NULLIFY(SP(I)%XLC_BU,SP(I)%YLC_BU,SP(I)%ZLC_BU,SP(I)%ZLL_BU, &
          SP(I)%ILC_BU,SP(I)%JLC_BU,SP(I)%KLC_BU,SP(I)%TOT_BU)

 ENDDO
 
 END SUBROUTINE TRACE_INIT_SP

 !###======================================================================
 SUBROUTINE TRACE_INIT_SPR(NG)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NG
 INTEGER :: I
 
 !## particle groups
 ALLOCATE(SPR(NG)); SPR%NPART=0; SPR%ICLR=0; SPR%IACT=0; SPR%IREV=0

 DO I=1,NG

  NULLIFY(SPR(I)%XLC   ,SPR(I)%YLC   ,SPR(I)%ZLC   ,SPR(I)%ZLL   , &
          SPR(I)%ILC   ,SPR(I)%JLC   ,SPR(I)%KLC   ,SPR(I)%TOT   )
  NULLIFY(SPR(I)%XLC_BU,SPR(I)%YLC_BU,SPR(I)%ZLC_BU,SPR(I)%ZLL_BU, &
          SPR(I)%ILC_BU,SPR(I)%JLC_BU,SPR(I)%KLC_BU,SPR(I)%TOT_BU)

 ENDDO
 
 END SUBROUTINE TRACE_INIT_SPR
 
 !###======================================================================
 SUBROUTINE TRACE_AL_SP(NPART,NG)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NPART,NG
 INTEGER :: I
 
 DO I=1,NG

  !## reals
  ALLOCATE(SP(I)%XLC(NPART), &
           SP(I)%YLC(NPART), &
           SP(I)%ZLC(NPART), &
           SP(I)%ZLL(NPART), &
           SP(I)%TOT(NPART))

  !## integers
  ALLOCATE(SP(I)%KLC(NPART), &
           SP(I)%JLC(NPART), &
           SP(I)%ILC(NPART))

 ENDDO
 
 END SUBROUTINE TRACE_AL_SP
 
 !###======================================================================
 SUBROUTINE TRACE_AL_SPR()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J
 
 CALL TRACE_DEAL_SPR(); IF(ALLOCATED(SPR))DEALLOCATE(SPR)
 
 ALLOCATE(SPR(SIZE(SP)))
 
 !## reset particles
 DO I=1,SIZE(SPR)

  !## reals
  ALLOCATE(SPR(I)%XLC(SP(I)%NPART), &
           SPR(I)%YLC(SP(I)%NPART), &
           SPR(I)%ZLC(SP(I)%NPART), &
           SPR(I)%ZLL(SP(I)%NPART), &
           SPR(I)%TOT(SP(I)%NPART))
  !## integers
  ALLOCATE(SPR(I)%KLC(SP(I)%NPART), &
           SPR(I)%JLC(SP(I)%NPART), &
           SPR(I)%ILC(SP(I)%NPART))
  
  SPR(I)%NPART=SP(I)%NPART
  
  DO J=1,SP(I)%NPART
   SPR(I)%XLC(J)=SP(I)%XLC(J)
   SPR(I)%YLC(J)=SP(I)%YLC(J)
   SPR(I)%ZLC(J)=SP(I)%ZLC(J)
   SPR(I)%ZLL(J)=SP(I)%ZLL(J)
   SPR(I)%KLC(J)=SP(I)%KLC(J)
   SPR(I)%ILC(J)=SP(I)%ILC(J)
   SPR(I)%JLC(J)=SP(I)%JLC(J)
   SPR(I)%TOT(J)=SP(I)%TOT(J)
  ENDDO

 ENDDO
     
 END SUBROUTINE TRACE_AL_SPR
 
 !###======================================================================
 SUBROUTINE TRACE_DEAL_SP()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 IF(.NOT.ALLOCATED(SP))RETURN
 
 !## reset particles
 DO I=1,SIZE(SP)
 
  IF(ASSOCIATED(SP(I)%XLC))DEALLOCATE(SP(I)%XLC)
  IF(ASSOCIATED(SP(I)%YLC))DEALLOCATE(SP(I)%YLC)
  IF(ASSOCIATED(SP(I)%ZLC))DEALLOCATE(SP(I)%ZLC)
  IF(ASSOCIATED(SP(I)%ILC))DEALLOCATE(SP(I)%ILC)
  IF(ASSOCIATED(SP(I)%JLC))DEALLOCATE(SP(I)%JLC)
  IF(ASSOCIATED(SP(I)%KLC))DEALLOCATE(SP(I)%KLC)
  IF(ASSOCIATED(SP(I)%ZLL))DEALLOCATE(SP(I)%ZLL)
  IF(ASSOCIATED(SP(I)%TOT))DEALLOCATE(SP(I)%TOT)

 ENDDO

 END SUBROUTINE TRACE_DEAL_SP
 
 !###======================================================================
 SUBROUTINE TRACE_DEAL_SPR()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 IF(.NOT.ALLOCATED(SPR))RETURN

 !## reset particles
 DO I=1,SIZE(SPR)

  IF(ASSOCIATED(SPR(I)%XLC))DEALLOCATE(SPR(I)%XLC)
  IF(ASSOCIATED(SPR(I)%YLC))DEALLOCATE(SPR(I)%YLC)
  IF(ASSOCIATED(SPR(I)%ZLC))DEALLOCATE(SPR(I)%ZLC)
  IF(ASSOCIATED(SPR(I)%ILC))DEALLOCATE(SPR(I)%ILC)
  IF(ASSOCIATED(SPR(I)%JLC))DEALLOCATE(SPR(I)%JLC)
  IF(ASSOCIATED(SPR(I)%KLC))DEALLOCATE(SPR(I)%KLC)
  IF(ASSOCIATED(SPR(I)%ZLL))DEALLOCATE(SPR(I)%ZLL)
  IF(ASSOCIATED(SPR(I)%TOT))DEALLOCATE(SPR(I)%TOT)
  
 ENDDO
 
 END SUBROUTINE TRACE_DEAL_SPR
 
 !###======================================================================
 LOGICAL FUNCTION TRACEREADBUDGET(IPER,IBATCH)
 !###======================================================================
 IMPLICIT NONE
 REAL,PARAMETER :: QCRIT=0.1  !## if bigger, it potential discharges water
 INTEGER,INTENT(IN) :: IBATCH,IPER
 CHARACTER(LEN=256) :: STRING
 INTEGER :: ILAY,IU,IROW,ICOL,ITYPE
 REAL :: QERROR,NODATAVALUE
 TYPE(WIN_MESSAGE) :: MESSAGE
 TYPE(IDFOBJ) :: IDFTMP
 
 TRACEREADBUDGET=.FALSE.

! if(.not.idfallocatex(idf))return
 CALL IDFCOPY(IDF,IDFTMP)

 IDFTMP%XMIN=IDF%XMIN-IDF%DX 
 IDFTMP%YMAX=IDF%YMAX
 IDFTMP%NCOL=IDF%NCOL+1
 IDFTMP%NROW=IDF%NROW
 
 !## read flux-right-face
 QX  =0.0
 DO ILAY=1,NLAY
  CALL WMESSAGEPEEK(ITYPE,MESSAGE)
  STRING='Reading BDGFRF L'//TRIM(ITOS(ILAY))//' '//TRIM(HFFNAME(1,ILAY,IPER))//'...'
  IF(IBATCH.EQ.1)WRITE(*,'(A)') TRIM(STRING)
  IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,STRING)
!  IF(.NOT.IDFREADSCALE(HFFNAME(1,ILAY,IPER),IDF,10,1,0.0,0))RETURN !## block-value
  IF(.NOT.IDFREADSCALE(HFFNAME(1,ILAY,IPER),IDFTMP,10,1,0.0,0))RETURN !## block-value
  DO IROW=1,IDF%NROW; DO ICOL=1,IDF%NCOL+1
   QX(ICOL,IROW,ILAY)=-IDFTMP%X(ICOL,IROW)  !## moved allready a single cell to the left
!   QX(ICOL+1,IROW,ILAY)=-IDF%X(ICOL,IROW) 
  ENDDO; ENDDO
 ENDDO

 IDFTMP%XMIN=IDF%XMIN
 IDFTMP%YMAX=IDF%YMAX+IDF%DX 
 IDFTMP%NCOL=IDF%NCOL
 IDFTMP%NROW=IDF%NROW+1

 !## read flux-front-face
 QY  =0.0
 DO ILAY=1,NLAY
  CALL WMESSAGEPEEK(ITYPE,MESSAGE)
  STRING='Reading BDGFFF L'//TRIM(ITOS(ILAY))//' '//TRIM(HFFNAME(2,ILAY,IPER))//'...'
  IF(IBATCH.EQ.1)WRITE(*,'(A)') TRIM(STRING)
  IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,STRING)
  IF(.NOT.IDFREADSCALE(HFFNAME(2,ILAY,IPER),IDFTMP,10,1,0.0,0))RETURN !## block-value
!  IF(.NOT.IDFREADSCALE(HFFNAME(2,ILAY,IPER),IDF,10,1,0.0,0))RETURN !## block-value
  DO ICOL=1,IDF%NCOL; DO IROW=1,IDF%NROW+1
!   QY(ICOL,IROW+1,ILAY)= IDF%X(ICOL,IROW) 
   QY(ICOL,IROW,ILAY)= IDFTMP%X(ICOL,IROW) 
  ENDDO; ENDDO
 ENDDO

 CALL IDFDEALLOCATEX(IDFTMP)

 !## read flux-lower-face
 QZ  =0.0
 IF(NLAY.GT.1)THEN
  DO ILAY=1,NLAY-1
   CALL WMESSAGEPEEK(ITYPE,MESSAGE)
   STRING='Reading BDGFLF L'//TRIM(ITOS(ILAY))//' '//TRIM(HFFNAME(3,ILAY,IPER))//'...'
   IF(IBATCH.EQ.1)WRITE(*,'(A)') TRIM(STRING)
   IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,STRING)
   IF(.NOT.IDFREADSCALE(HFFNAME(3,ILAY,IPER),IDF,10,1,0.0,0))RETURN !## block-value
   DO IROW=1,IDF%NROW; DO ICOL=1,IDF%NCOL
    QZ(ICOL,IROW,ILAY+1)=IDF%X(ICOL,IROW) 
   ENDDO; ENDDO
  ENDDO
 ENDIF

 !##initialize qss - as nett-term waterbalance
 QSS=0.0
 DO ILAY=1,NLAY
  DO IROW=1,IDF%NROW
   DO ICOL=1,IDF%NCOL
    
    !## reset ibound 
    IF(IBOUND(ICOL,IROW,ILAY).EQ. 2000)IBOUND(ICOL,IROW,ILAY)=1
    IF(IBOUND(ICOL,IROW,ILAY).EQ.-2000)IBOUND(ICOL,IROW,ILAY)=1
    
    QERROR=QX(ICOL,IROW,ILAY)-QX(ICOL+1,IROW,ILAY)- &
           QY(ICOL,IROW,ILAY)+QY(ICOL,IROW+1,ILAY)- &
           QZ(ICOL,IROW,ILAY)+QZ(ICOL,IROW,ILAY+1)
    QSS(ICOL,IROW,ILAY)=-QERROR
    !## original code flags all buff().lt.0 ! 
    IF(QSS(ICOL,IROW,ILAY).LT.-QCRIT.AND.IBOUND(ICOL,IROW,ILAY).NE.0)IBOUND(ICOL,IROW,ILAY)=-2000
    IF(QSS(ICOL,IROW,ILAY).GT. QCRIT.AND.IBOUND(ICOL,IROW,ILAY).NE.0)IBOUND(ICOL,IROW,ILAY)= 2000
   ENDDO
  ENDDO
 ENDDO
 
 TRACEREADBUDGET=.TRUE.

 END FUNCTION TRACEREADBUDGET

 !###======================================================================
 SUBROUTINE TRACEIREV()
 !###======================================================================
 IMPLICIT NONE

 QX =-1.0*QX
 QY =-1.0*QY
 QZ =-1.0*QZ

 END SUBROUTINE TRACEIREV

 !###======================================================================
 LOGICAL FUNCTION TRACEDATIN(NCONS,IBATCH)
 !###======================================================================
 IMPLICIT NONE
 REAL,PARAMETER :: MINTHICKNESS=0.01 !## OTHERWISE TRACING IS PROBLEM, DIVIDING BY ZERO AND NO PARTICLE BEHAVIOR IN Z-DIRECTION
 INTEGER,INTENT(IN) :: IBATCH
 INTEGER,INTENT(OUT) :: NCONS
 CHARACTER(LEN=256) :: STRING
 REAL :: APOR,XZTOP,XZBOT,NODATAVALUE
 INTEGER :: ILAY,ICOL,IROW,IU,IOS,IBND,ITYPE
 LOGICAL :: LEX
 TYPE(WIN_MESSAGE) :: MESSAGE
 
 TRACEDATIN=.FALSE.

 !## all modellayers do have confining beds by default
 NCON=1

 IF(IBATCH.EQ.0)CALL WINDOWSELECT(0)

 !## read iboundary idf-files
 DO ILAY=1,NLAY
  CALL WMESSAGEPEEK(ITYPE,MESSAGE)
  STRING=ITBFNAME(1,ILAY)
  READ(STRING,*,IOSTAT=IOS) IBND
  IF(IOS.EQ.0)THEN
   IBOUND(1:IDF%NCOL,1:IDF%NROW,ILAY)=IBND
   STRING='Constant value ['//TRIM(ITOS(IBND))//'] for IBOUND L'//TRIM(ITOS(ILAY))
  ELSE
   STRING='Reading IBOUND L'//TRIM(ITOS(ILAY))//' '//TRIM(ITBFNAME(1,ILAY))//'...'
   LEX=.TRUE.
   IF(ILAY.GT.1)THEN
    LEX=.TRUE.
    IF(TRIM(ITBFNAME(1,ILAY)).EQ.TRIM(ITBFNAME(1,ILAY-1)))LEX=.FALSE.
   ENDIF
   IF(LEX)THEN
    IF(.NOT.IDFREADSCALE(ITBFNAME(1,ILAY),IDF,1,1,0.0,0))RETURN !## scale boundary
    IBOUND(:,:,ILAY)=INT(IDF%X)
   ELSE
    STRING='Reusing '//TRIM(ITBFNAME(1,ILAY-1))
    IBOUND(:,:,ILAY)=IBOUND(:,:,ILAY-1)
   ENDIF
  ENDIF
  IF(IBATCH.EQ.1)WRITE(*,*) TRIM(STRING)
  IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,STRING)

  DO IROW=1,IDF%NROW
   DO ICOL=1,IDF%NCOL

    IF(IBOUND(ICOL,IROW,ILAY).LT.0)THEN
     IBOUND(ICOL,IROW,ILAY)=-1
     !## POTENTIAL DISCHARGE CELLS ARE FLAGGED IN THE IBOUND ARRAY
     !## BY MULTIPLYING IBOUND VALUE BY 1000. THIS LOOP FLAGS CHANGING
     !## CELLS THAT HAVE NET DISCHARGE.
     IBOUND(ICOL,IROW,ILAY)=1000*IBOUND(ICOL,IROW,ILAY)
    ENDIF
    IF(IBOUND(ICOL,IROW,ILAY).GT.0)IBOUND(ICOL,IROW,ILAY)= 1
   END DO
  END DO
  !## FLAG SURROUNDING MODEL AS DISCHARGE AREA!!!!

 ENDDO

 !## read vertical coordinate data (TOP)
 DO ILAY=1,NLAY
  CALL WMESSAGEPEEK(ITYPE,MESSAGE)
  STRING=ITBFNAME(2,ILAY)
  READ(STRING,*,IOSTAT=IOS) XZTOP
  IF(IOS.EQ.0)THEN
   ZTOP(1:IDF%NCOL,1:IDF%NROW,ILAY)=XZTOP
   STRING='Constant value ['//TRIM(RTOS(XZTOP,'F',2))//'] for ZTOP L'//TRIM(ITOS(ILAY))
  ELSE
   STRING='Reading TOP L'//TRIM(ITOS(ILAY))//' '//TRIM(ITBFNAME(2,ILAY))//'...'
   IF(.NOT.IDFREADSCALE(ITBFNAME(2,ILAY),IDF,2,1,0.0,0))RETURN !## scale mean
   ZTOP(:,:,ILAY)=IDF%X
   !## adjust boundary whenever top
   DO IROW=1,IDF%NROW; DO ICOL=1,IDF%NCOL
    IF(ZTOP(ICOL,IROW,ILAY).EQ.IDF%NODATA)IBOUND(ICOL,IROW,ILAY)=0
   ENDDO; ENDDO
  ENDIF
  IF(IBATCH.EQ.1)WRITE(*,'(A)') TRIM(STRING)
  IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,STRING)
 ENDDO

 !## read vertical coordinate data (BOT)
 DO ILAY=1,NLAY
  CALL WMESSAGEPEEK(ITYPE,MESSAGE)
  STRING=ITBFNAME(3,ILAY)
  READ(STRING,*,IOSTAT=IOS) XZBOT
  IF(IOS.EQ.0)THEN
   ZBOT(1:IDF%NCOL,1:IDF%NROW,ILAY)=XZBOT
   STRING='Constant value ['//TRIM(RTOS(XZBOT,'F',2))//'] for ZBOT L'//TRIM(ITOS(ILAY))
  ELSE
   STRING='Reading BOT L'//TRIM(ITOS(ILAY))//' '//TRIM(ITBFNAME(3,ILAY))//'...'
   IF(.NOT.IDFREADSCALE(ITBFNAME(3,ILAY),IDF,2,1,0.0,0))RETURN !## scale mean
   ZBOT(:,:,ILAY)=IDF%X   
   !## adjust boundary whenever top
   DO IROW=1,IDF%NROW; DO ICOL=1,IDF%NCOL
    IF(ZBOT(ICOL,IROW,ILAY).EQ.IDF%NODATA)IBOUND(ICOL,IROW,ILAY)=0
   ENDDO; ENDDO
  ENDIF
  IF(IBATCH.EQ.1)WRITE(*,'(A)') TRIM(STRING)
  IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,STRING)
 ENDDO

 !## assign porosity
 DO ILAY=1,NLAY
  CALL WMESSAGEPEEK(ITYPE,MESSAGE)
  STRING=ITBFNAME(4,ILAY)
  READ(STRING,*,IOSTAT=IOS) APOR
  IF(IOS.EQ.0)THEN
   POR(1:IDF%NCOL,1:IDF%NROW,ILAY)=APOR
   STRING='Constant value ['//TRIM(RTOS(APOR,'F',2))//'] for POR_AQF L'//TRIM(ITOS(ILAY))
  ELSE
   STRING='Reading POR_AQF L'//TRIM(ITOS(ILAY))//' '//TRIM(ITBFNAME(4,ILAY))//'...'
   IF(.NOT.IDFREADSCALE(ITBFNAME(4,ILAY),IDF,2,1,0.0,0))RETURN !## scale mean
   POR(:,:,ILAY)=IDF%X   
   !## adjust boundary whenever por
   DO IROW=1,IDF%NROW; DO ICOL=1,IDF%NCOL
    IF(POR(ICOL,IROW,ILAY).EQ.IDF%NODATA)IBOUND(ICOL,IROW,ILAY)=0
   ENDDO; ENDDO
  ENDIF
  IF(IBATCH.EQ.1)WRITE(*,'(A)') TRIM(STRING)
  IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,STRING)
 ENDDO

 DO ILAY=1,NLAY-1
  CALL WMESSAGEPEEK(ITYPE,MESSAGE)
  STRING=ITBFNAME(5,ILAY)
  READ(STRING,*,IOSTAT=IOS) APOR
  IF(IOS.EQ.0)THEN
   POR(1:IDF%NCOL,1:IDF%NROW,NLAY+ILAY)=APOR
   STRING='Constant value ['//TRIM(RTOS(APOR,'F',2))//'] for POR_AQT L'//TRIM(ITOS(ILAY))
  ELSE
   STRING='Reading POR_AQT L'//TRIM(ITOS(ILAY))//' '//TRIM(ITBFNAME(5,ILAY))//'...'
   IF(.NOT.IDFREADSCALE(ITBFNAME(5,ILAY),IDF,2,1,0.0,0))RETURN !## scale mean
   POR(:,:,NLAY+ILAY)=IDF%X   
   !## adjust boundary whenever por
   DO IROW=1,IDF%NROW; DO ICOL=1,IDF%NCOL
    IF(POR(ICOL,IROW,NLAY+ILAY).EQ.IDF%NODATA)THEN; IBOUND(ICOL,IROW,ILAY)=0; IBOUND(ICOL,IROW,ILAY+1)=0;ENDIF
   ENDDO; ENDDO
  ENDIF
  IF(IBATCH.EQ.1)WRITE(*,'(A)') TRIM(STRING)
  IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,STRING)
 ENDDO

 !## check top/bot consequentie - minimal thickness=minthickness
 NCONS=0
 DO IROW=1,IDF%NROW
  DO ICOL=1,IDF%NCOL
   DO ILAY=1,NLAY
    IF(IBOUND(ICOL,IROW,ILAY).NE.0)THEN
     IF(ZTOP(ICOL,IROW,ILAY).LT.ZBOT(ICOL,IROW,ILAY)+MINTHICKNESS)THEN
      NCONS=NCONS+1
      ZBOT(ICOL,IROW,ILAY)=ZTOP(ICOL,IROW,ILAY)-MINTHICKNESS
     ENDIF
     IF(ILAY.LT.NLAY)THEN
      IF(IBOUND(ICOL,IROW,ILAY+1).NE.0)THEN
       IF(ZBOT(ICOL,IROW,ILAY).LT.ZTOP(ICOL,IROW,ILAY+1)+MINTHICKNESS)THEN
        NCONS=NCONS+1
        ZTOP(ICOL,IROW,ILAY+1)=ZBOT(ICOL,IROW,ILAY)-MINTHICKNESS
       ENDIF
      ENDIF
     ENDIF
    ENDIF
   ENDDO
  ENDDO
 ENDDO

 IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'')

! IF(.not.IDFALLOCATEX(IDF))stop
! DO ILAY=1,NLAY
!  idf%x=REAL(IBOUND(:,:,ilay))
!  IF(.not.idfwrite(idf,TRIM(PREFVAL(1))//'\ibound_l'//TRIM(ITOS(ILAY))//'.idf',1))then
!  endif
! END DO
! DO ILAY=1,NLAY
!  idf%x=ZTOP(:,:,ilay)
!  IF(.not.idfwrite(idf,TRIM(PREFVAL(1))//'\ztop_l'//TRIM(ITOS(ILAY))//'.idf',1))then
!  endif
! END DO
! DO ILAY=1,NLAY
!  idf%x=Zbot(:,:,ilay)
!  IF(.not.idfwrite(idf,TRIM(PREFVAL(1))//'\zBOT_l'//TRIM(ITOS(ILAY))//'.idf',1))then
!  endif
! END DO
! DO ILAY=1,(NLAY*2)-1
!  idf%x=POR(:,:,ilay)
!  IF(.not.idfwrite(idf,TRIM(PREFVAL(1))//'\por_l'//TRIM(ITOS(ILAY))//'.idf',1))then
!  endif
! END DO

 TRACEDATIN=.TRUE.

 END FUNCTION TRACEDATIN

 !###====================================================================
 SUBROUTINE TRACEDELRC()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I,II

 IF(IDF%IEQ.EQ.0)THEN

  !#fill global coordinates
  DELR(0)=0.0 
  DO I=1,IDF%NCOL
   DELR(I)=REAL(I)*IDF%DX
  END DO
  DO I=0,IDF%NCOL
   DELR(I)=IDF%XMIN+DELR(I)
  ENDDO
  
  DELC(0)=0.0 
  DO I=1,IDF%NROW
   DELC(I)=-REAL(I)*IDF%DY
  END DO
  DO I=0,IDF%NROW
   DELC(I)=IDF%YMAX+DELC(I)
  ENDDO
  
  !#fill local coordinates
  DO I=1,IDF%NCOL
   LDELR(I)=REAL(I)*IDF%DX
  END DO
  II=0
  DO I=IDF%NROW,1,-1 
   II=II+1
   LDELC(I)=REAL(II)*IDF%DY
  ENDDO

  DELX=IDF%DX
  DELY=IDF%DY

 ELSE

  DELR(0)=IDF%XMIN
  DO I=1,IDF%NCOL
   !## fill global coordinates
   DELR(I) =IDF%SX(I)
   !## delta x
   DELX(I) =DELR(I)-DELR(I-1)
   !## fill local coordinates
   LDELR(I)=DELR(I)-IDF%XMIN
  END DO
  DELC(0)=IDF%YMAX
  DO I=1,IDF%NROW
   !## fill global coordinates
   DELC(I) =IDF%SY(I)
   !## delta y
   DELY(I) =DELC(I-1)-DELC(I)
   !## fill local coordinates
   LDELC(I)=DELC(I-1)-IDF%YMIN
  END DO

 ENDIF
 
 END SUBROUTINE

END MODULE MOD_PLINES_TRACE

