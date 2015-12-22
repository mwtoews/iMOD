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
MODULE MOD_PLINES_TRACE

USE WINTERACTER
USE RESOURCE
USE OPENGL
USE IMOD, ONLY : IDFINIT
USE MODPLOT, ONLY : MPW,MP
USE MOD_ASC2IDF_PAR, ONLY : IDFFILE,IGRIDFUNC,CS,NODATA,XYZFNAMES,IXCOL,IYCOL,IZCOL
USE MOD_ASC2IDF, ONLY : ASC2IDF_INT_MAIN
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_UTL, ONLY : ITOS,UTL_GETUNIT,RTOS,JD,UTL_IDATETOJDATE,UTL_WAITMESSAGE,UTL_IDFGETDATE,UTL_CAP, &
           UTL_GETUNIQUE_CHAR,UTL_CREATEDIR,UTL_IDFSNAPTOGRID,UTL_WSELECTFILE
USE MOD_IDF, ONLY : IDFREAD,IDFDEALLOCATEX,IDFALLOCATEX,IDFWRITE,IDFIROWICOL,IDFREADSCALE,IDFGETLOC,IDFCOPY
USE MOD_PLINES_PAR
USE MOD_PLINES_READ, ONLY : TRACEREADBLOCK_R,TRACEREADBLOCK_I 
USE MOD_PLINES_SP, ONLY : TRACEPREPARESP,TRACEREADSP
USE MOD_PLINES_FLOLIN, ONLY : FLOLIN
USE MOD_OSD, ONLY : OSD_OPEN,OSD_IOSTAT_MSG
USE MOD_MANAGER, ONLY : MANAGERDELETE
USE MOD_DEMO_PAR
USE MOD_3D_PAR, ONLY : PLLISTINDEX,PLLISTCLR,PLLISTAGE,STPLISTINDEX,MIDPOS,TOP,BOT,XYZAXES,IDFPLOT,NSPG,SPGCLR,SPGPOS,MAXNSPG,IPATHLINE_3D

CONTAINS

 !###======================================================================
 LOGICAL FUNCTION TRACE_3D_INIT()
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=256) :: RUNFILE
 INTEGER :: IBATCH,ILAY,IPLOT
  
 TRACE_3D_INIT=.FALSE.

! IF(.NOT.UTL_WSELECTFILE('iMODPATH runfile (*.run)|*.run|', &
!                  LOADDIALOG+PROMPTON+DIRCHANGE+APPENDEXT, &
!                  RUNFILE,'Select iMODPATH runfile (*.run)'))RETURN
 
 RUNFILE='d:\iMOD-Gebruikersdag\IMOD_USER\RUNFILES\imodpath.run'
 RUNFILE='d:\IMOD-MODELS\IBRAHYM_V2.0\iMOD\IMOD_USER\RUNFILES\erfverband.run'
 
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
 PL%SPCOLOR=WRGB(200,10,10) !## initial olour of start-locations
 
 ALLOCATE(IVISIT(IDF%NCOL*IDF%NROW*NLAY)); IVISIT=INT(0,1)
 ALLOCATE(LVISIT(IDF%NCOL*IDF%NROW*MIN(2,NLAY))); LVISIT=0

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
 
 !## maximal number of particles to be traced
 NPART=10000; CALL TRACEALPART()

 CALL WDIALOGLOAD(ID_D3DSETTINGS_LAYER,ID_D3DSETTINGS_LAYER)
 CALL WDIALOGLOAD(ID_D3DSETTINGS_LOCATION,ID_D3DSETTINGS_LOCATION)
 CALL WDIALOGLOAD(ID_D3DSETTINGS_SINKS,ID_D3DSETTINGS_SINKS)

 CALL WDIALOGLOAD(ID_D3DSETTINGS_PART,ID_D3DSETTINGS_PART)
 MAXNSPG=WINFOGRID(IDF_GRID1,GRIDROWSMAX)

 !## store colour per group for startpoints - red on default
 ALLOCATE(SPGCLR(MAXNSPG)); SPGCLR=WRGB(255,0,0)
 !## store position in startpoit where group starts
 ALLOCATE(SPGPOS(MAXNSPG,2)); SPGPOS=0
 NSPG=0
 ALLOCATE(STPLISTINDEX(MAXNSPG)); STPLISTINDEX=0

 DEMO%IDEMO=2; DEMO%CONFLAG=4; DEMO%ACCFLAG=3; DEMO%IFILL=1
 
 CALL WMENUSETSTATE(ID_INTERACTIVEPATHLINES,2,1)
 CALL WMENUSETSTATE(ID_INTERACTIVEPATHLINES,1,0)
 
 TRACE_3D_INIT=.TRUE.
 
 END FUNCTION TRACE_3D_INIT
 
 !###======================================================================
 SUBROUTINE TRACE_3D_RESET() 
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,N
 REAL :: T
 
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
 !## number of particles to be followed in time
 CALL WDIALOGGETINTEGER(IDF_INTEGER4,N)

 !## drawinglist index
 ALLOCATE(PLLISTINDEX(N,MAXNSPG)); PLLISTINDEX=0
 !## colour fraction
 ALLOCATE(PLLISTCLR(N)); PLLISTCLR=0
 !## age
 ALLOCATE(PLLISTAGE(N)); PLLISTAGE=0.0
 
 PL%TCUR=0.0
 PL%NPER=0

 CALL WDIALOGPUTREAL(IDF_REAL7,PL%TCUR) 

 !## reset particles
 DO I=1,PL%NPART
  XLC(I,2)  =XLC(I,1)
  YLC(I,2)  =YLC(I,1)
  ZLC(I,2)  =ZLC(I,1)
  ZLL(I,2)  =ZLL(I,1)
  KLC(I,2)  =KLC(I,1)
  ILC(I,2)  =ILC(I,1)
  JLC(I,2)  =JLC(I,1)
  TOT(I,2)  =TOT(I,1)
  MAXILAY(I)=0
  SLAY(I)   =0
 ENDDO
     
 END SUBROUTINE TRACE_3D_RESET

 !###======================================================================
 SUBROUTINE TRACE_3D_STARTPOINTS() 
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,K,NX,NZ,N,ISP,IROW,ICOL,ILAY,IOPT,ISTRONG,IC1,IC2,IR1,IR2
 REAL :: X,Y,Z,DX,DY,DZ,XC,YC,ZC,Q,QERROR
 REAL,DIMENSION(:),ALLOCATABLE :: XSP,YSP,ZSP
 TYPE(IDFOBJ) :: PIDF
 LOGICAL :: LEX
 
 !## get location of mouse
 CALL WDIALOGSELECT(ID_D3DSETTINGS_TAB8)
 CALL WDIALOGGETRADIOBUTTON(IDF_RADIO5,ISP)

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

  !## spatial distributed via an IDF-file
  CASE (2)
   CALL WDIALOGGETMENU(IDF_MENU1,I)  
   CALL IDFCOPY(IDF,PIDF)
   IF(.NOT.IDFREADSCALE(IDFPLOT(IDFPLOT(I)%DISP_ILIST)%FNAME,PIDF,2,0,0.0,0))RETURN
   DO I=1,2
    N=0
    DO IROW=1,PIDF%NROW; DO ICOL=1,PIDF%NCOL 
     IF(PIDF%X(ICOL,IROW).NE.PIDF%NODATA)THEN
      N=N+1
      IF(I.EQ.2)THEN
       CALL IDFGETLOC(PIDF,IROW,ICOL,XSP(N),YSP(N))
       ZSP(N)=PIDF%X(ICOL,IROW)
      ENDIF
     ENDIF
    ENDDO; ENDDO
    IF(I.EQ.1)ALLOCATE(XSP(N),YSP(N),ZSP(N))
   ENDDO
   CALL IDFDEALLOCATEX(PIDF)

  !## sinks
  CASE (3)
   CALL WDIALOGGETREAL(IDF_REAL10,Q)
   CALL WDIALOGGETMENU(IDF_MENU2,IOPT)
   CALL WDIALOGGETCHECKBOX(IDF_CHECK5,ISTRONG)
   
   DO I=1,2
    N=0
    DO ILAY=1,NLAY; DO IROW=1,IDF%NROW; DO ICOL=1,IDF%NCOL
    
     !## skip ibound
     IF(IBOUND(ICOL,IROW,ILAY).LE.0)CYCLE
        
     !## waterbalance error
     QERROR=QX(ICOL,IROW,ILAY)-QX(ICOL+1,IROW,ILAY)- &
            QY(ICOL,IROW,ILAY)+QY(ICOL,IROW+1,ILAY)- &
            QZ(ICOL,IROW,ILAY)+QZ(ICOL,IROW,ILAY+1)

     LEX=.FALSE.
     SELECT CASE (IOPT)
      CASE (1) !## >=
       IF(QERROR.GE.Q)LEX=.TRUE.
      CASE (2) !## <=
       IF(QERROR.LE.Q)LEX=.TRUE.
     END SELECT

     !## check whether strong-sink
     IF(ISTRONG.EQ.1)THEN
      IF(QX(ICOL,IROW,ILAY).GE.0.0.AND.QX(ICOL+1,IROW,ILAY).LE.0.0.AND. &
         QY(ICOL,IROW,ILAY).GE.0.0.AND.QY(ICOL,IROW+1,ILAY).LE.0.0.AND. &
         QZ(ICOL,IROW,ILAY).GE.0.0.AND.QZ(ICOL,IROW,ILAY+1).LE.0.0)LEX=.FALSE.
     ENDIF

     IF(LEX)THEN
      N=N+1
      IF(I.EQ.2)THEN
       CALL IDFGETLOC(IDF,IROW,ICOL,XSP(N),YSP(N))
       ZSP(N)=0.5*(ZBOT(ICOL,IROW,ILAY)+ZTOP(ICOL,IROW,ILAY))
      ENDIF
     ENDIF
    
    ENDDO; ENDDO; ENDDO
    IF(I.EQ.1)ALLOCATE(XSP(N),YSP(N),ZSP(N))
   ENDDO

  !## border
  CASE (4)
   CALL WDIALOGGETRADIOBUTTON(IDF_RADIO12,IOPT) 
   CALL WDIALOGSELECT(ID_D3DSETTINGS_LAYER)
   CALL WDIALOGGETINTEGER(IDF_INTEGER8,NZ)
   
   IC1=1; IC2=IDF%NCOL; IR1=1; IR2=IDF%NROW
   
   IF(IOPT.EQ.1)IC2=1;        IF(IOPT.EQ.2)IR2=1
   IF(IOPT.EQ.3)IC1=IDF%NCOL; IF(IOPT.EQ.4)IR1=IDF%NROW

   N=NZ*NLAY*MAX(IC2,IR2); ALLOCATE(XSP(N),YSP(N),ZSP(N))

   N=0; DO ICOL=IC1,IC2; DO IROW=IR1,IR2; DO ILAY=1,NLAY
    DZ=(ZTOP(ICOL,IROW,ILAY)-ZBOT(ICOL,IROW,ILAY))/REAL(NZ)
    Z = ZTOP(ICOL,IROW,ILAY)+(0.5*DZ)
    CALL IDFGETLOC(IDF,IROW,ICOL,XC,YC)
    DO J=1,NZ
     N=N+1; Z=Z-DZ; XSP(N)=XC; YSP(N)=YC; ZSP(N)=Z
    ENDDO
   ENDDO; ENDDO; ENDDO

  !## file
  CASE (5)
  
 END SELECT
 
 NSPG=NSPG+1
 
 SPGPOS(NSPG,1)=PL%NPART
 !## get new location/particle - click from the 3d tool
 CALL TRACE_3D_STARTPOINTS_ASSIGN(XSP,YSP,ZSP)
 SPGPOS(NSPG,2)=PL%NPART
 !## assign current colour to group
 SPGCLR(NSPG)=PL%SPCOLOR
 
 !## deallocate memory for temporary storage of locations
 DEALLOCATE(XSP,YSP,ZSP)

 CALL WDIALOGSELECT(ID_D3DSETTINGS_TAB8)
 IF(NSPG.GT.MAXNSPG)CALL WDIALOGFIELDSTATE(ID_ADD,0)
 
 END SUBROUTINE TRACE_3D_STARTPOINTS
 
 !###======================================================================
 SUBROUTINE TRACE_3D_STARTPOINTS_ASSIGN(XSP,YSP,ZSP)
 !###======================================================================
 IMPLICIT NONE
 REAL,INTENT(IN),DIMENSION(:) :: XSP,YSP,ZSP
 INTEGER :: IROW,ICOL,ILAY,I,J,K,N
 REAL :: XC,YC,ZC,ZL,DZ

 DO I=1,SIZE(XSP)

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
   !## skip inactive/constant head cells
   IF(IBOUND(ICOL,IROW,ILAY).LE.0)CYCLE
   
   ZL=0.0

   !## inside current modellayer
   IF(ZC.LE.ZTOP(ICOL,IROW,ILAY).AND.ZC.GE.ZBOT(ICOL,IROW,ILAY))THEN
    !## compute local z: top (zl=1); mid (zl=0.5); bot (zl=0)
    DZ=ZTOP(ICOL,IROW,ILAY)-ZBOT(ICOL,IROW,ILAY)
    IF(DZ.NE.0.0)ZL=(ZC-ZBOT(ICOL,IROW,ILAY))/DZ
   ENDIF
   IF(ZL.EQ.0.0.AND.ILAY.LT.NLAY)THEN
    !## skip inactive/constant head cells under layer
    IF(IBOUND(ICOL,IROW,ILAY+1).LE.0)CYCLE
    !## inside interbed between modellayers
    IF(ZC.LT.ZBOT(ICOL,IROW,ILAY).AND.ZC.GT.ZTOP(ICOL,IROW,ILAY+1))THEN
     !## compute local z: top (zl=1); mid (zl=0.5); bot (zl=0)
     DZ=ZBOT(ICOL,IROW,ILAY)-ZTOP(ICOL,IROW,ILAY+1)
     IF(DZ.NE.0.0)ZL=(ZTOP(ICOL,IROW,ILAY+1)-ZC)/DZ
    ENDIF
   ENDIF

   IF(ZL.NE.0.0)THEN
    !## count number of particles
    PL%NPART=PL%NPART+1
    
    IF(PL%NPART.GT.SIZE(XLC,1))THEN
     N=PL%NPART+999
     ALLOCATE(XLC_BU(N,2),YLC_BU(N,2),ZLC_BU(N,2),ZLL_BU(N,2),KLC_BU(N,2),ILC_BU(N,2),JLC_BU(N,2),TOT_BU(N,2),SLAY_BU(N),MAXILAY_BU(N))
     DO K=1,PL%NPART-1
      DO J=1,2
       XLC_BU(K,J)=XLC(K,J)
       YLC_BU(K,J)=YLC(K,J)
       ZLC_BU(K,J)=ZLC(K,J)
       ZLL_BU(K,J)=ZLL(K,J)
       KLC_BU(K,J)=KLC(K,J)
       ILC_BU(K,J)=ILC(K,J)
       JLC_BU(K,J)=JLC(K,J)
       TOT_BU(K,J)=TOT(K,J)
      ENDDO
      SLAY_BU(K)   =SLAY(K)
      MAXILAY_BU(K)=MAXILAY(K)
     ENDDO
     DEALLOCATE(XLC,YLC,ZLC,ZLL,KLC,ILC,JLC,TOT,SLAY,MAXILAY)
     XLC    =>XLC_BU
     YLC    =>YLC_BU
     ZLC    =>ZLC_BU
     ZLL    =>ZLL_BU
     KLC    =>KLC_BU
     ILC    =>ILC_BU
     JLC    =>JLC_BU
     TOT    =>TOT_BU
     SLAY   =>SLAY_BU
     MAXILAY=>MAXILAY_BU
    ENDIF
    
    XLC(PL%NPART,1)  =XC-IDF%XMIN
    YLC(PL%NPART,1)  =YC-IDF%YMIN
    ZLC(PL%NPART,1)  =ZC
    ZLL(PL%NPART,1)  =ZL
    KLC(PL%NPART,1)  =ILAY
    ILC(PL%NPART,1)  =IROW
    JLC(PL%NPART,1)  =ICOL
    TOT(PL%NPART,1)  =0.0
    SLAY(PL%NPART)   =0
    MAXILAY(PL%NPART)=0
    EXIT
   ENDIF
  ENDDO

 ENDDO

 END SUBROUTINE TRACE_3D_STARTPOINTS_ASSIGN

 !###======================================================================
 SUBROUTINE TRACE_3D_STARTPOINTS_SHOW()
 !###======================================================================
 IMPLICIT NONE
 REAL :: DX,DY
 REAL(KIND=GLFLOAT) :: X,Y,Z
 INTEGER :: I,IG
  
 DX=(TOP%X-BOT%X)/2.0_GLFLOAT/XYZAXES(1)
 DY=(TOP%Y-BOT%Y)/2.0_GLFLOAT/XYZAXES(2)

 !## clear all
 DO IG=1,MAXNSPG
  !## open drawing list per timestep/call
  IF(STPLISTINDEX(IG).NE.0)CALL GLDELETELISTS(STPLISTINDEX(IG),1_GLSIZEI); STPLISTINDEX(IG)=0
 ENDDO
 
 IF(PL%NPART.LE.0)THEN
  NSPG=0; SPGPOS=0
  CALL WDIALOGSELECT(ID_D3DSETTINGS_TAB8)
  CALL WDIALOGPUTINTEGER(IDF_INTEGER5,PL%NPART)
  CALL WDIALOGPUTINTEGER(IDF_INTEGER11,NSPG)
  RETURN
 ENDIF
 
 !## create seperate list for all of the particles
 DO IG=1,NSPG

  !## list index for,  !## start new drawing list
  STPLISTINDEX(IG)=GLGENLISTS(1); CALL GLNEWLIST(STPLISTINDEX(IG),GL_COMPILE)
 
  CALL GLBEGIN(GL_POINTS)

  DO I=SPGPOS(IG,1)+1,SPGPOS(IG,2)   
   !## current position
   X= XLC(I,1)+IDF%XMIN; Y= YLC(I,1)+IDF%YMIN; Z= ZLC(I,1)
   X=(X-MIDPOS%X)/DX; Y=(Y-MIDPOS%Y)/DY
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
 INTEGER :: I,IPART,IDSCH,NPACT,NPER,IG
 REAL :: TIME,TTMAX,MAXVELOCITY
 
 TRACE_3D_COMPUTE=.FALSE.

 !## maximal time of simulation - equal to refreshing rate or something to be trickered by the 3d tool
 MAXVELOCITY=0.0

 CALL WDIALOGSELECT(ID_D3DSETTINGS_TAB8)
 CALL WDIALOGGETREAL(IDF_REAL4,PL%TMAX); PL%TMAX=PL%TMAX*DYEAR
 CALL WDIALOGGETREAL(IDF_REAL5,PL%TDEL); PL%TDEL=PL%TDEL*DYEAR
 CALL WDIALOGGETREAL(IDF_REAL6,PL%TINC)
 CALL WDIALOGGETREAL(IDF_REAL7,PL%TCUR); PL%TCUR=PL%TCUR*DYEAR

 NPER=PL%NPER
 WRITE(*,*) PL%NPER,NPER
 !## increase number of timesteps
 PL%NPER=PL%NPER+1
 NPER=NPER+1
 WRITE(*,*) PL%NPER,NPER

 PL%NPER=NPER
 WRITE(*,*) PL%NPER,NPER

 !## shift all one position
 DO I=1,SIZE(PLLISTCLR); PLLISTCLR(I)=PLLISTCLR(I)+1; ENDDO
 
 !## restart in stack
 IF(PL%NPER.GT.SIZE(PLLISTINDEX,1))PL%NPER=1

 !## get next timestep
 TTMAX=PL%TCUR+PL%TDEL**PL%TINC
 !## maximize it for tmax
 TTMAX=MIN(TTMAX,PL%TMAX)

 !## age of current drawing list
 PLLISTAGE(PL%NPER)=TTMAX
 !## position in colouring of current drawing list
 PLLISTCLR(PL%NPER)=1
 
 !## count number of active particles
 NPACT=0; IMODE=0; IMODE(1)=1
 
 DO IG=1,NSPG

  !## open drawing list per timestep/call
  IF(PLLISTINDEX(PL%NPER,IG).NE.0)CALL GLDELETELISTS(PLLISTINDEX(PL%NPER,IG),1_GLSIZEI)
  !## list index for,  !## start new drawing list
  PLLISTINDEX(PL%NPER,IG)=GLGENLISTS(1); CALL GLNEWLIST(PLLISTINDEX(PL%NPER,IG),GL_COMPILE)
 
  !## points
  IF(PL%ITYPE.EQ.2)CALL GLBEGIN(GL_POINTS)

  DO IPART=SPGPOS(IG,1)+1,SPGPOS(IG,2) !1,PL%NPART

   !## time in days
   TIME=TOT(IPART,2)
   !## trace selected particle, not yet discharged!
   IF(KLC(IPART,2).GT.0)THEN

    !# lines
    IF(PL%ITYPE.EQ.1)CALL GLBEGIN(GL_LINE_STRIP)
     
    NPACT=NPACT+1
    CALL FLOLIN(IPART,IMODE(1),TIME,TTMAX,IDSCH,JLC(IPART,2),ILC(IPART,2),KLC(IPART,2),   &
                XLC(IPART,2),YLC(IPART,2),ZLC(IPART,2),ZLL(IPART,2),IBOUND,ZBOT, &
                ZTOP,LDELR,LDELC,QX,QY,QZ,QSS,POR,NCON,IDF%NCOL,IDF%NROW,NLAY,  &
                NLPOR,IDF%NCOL*IDF%NROW*NLAY,NCP1,NRP1,NLP1,ISNK,IREV,FRAC,IMODE(1),   &
                ISS,MAXVELOCITY,DELX,DELY,MAXILAY(IPART),IVISIT,LVISIT,NVISIT) 

    !## lines
    IF(PL%ITYPE.EQ.1)CALL GLEND()

    !## clean visited places that have been visited before
    DO I=1,NVISIT; IVISIT(LVISIT(I))=INT(0,1); ENDDO

    !## time in days!
    TOT(IPART,2)=TIME

    !## reached tmax - particle can continue next step
    IF(IDSCH.EQ.7)IDSCH=0
    !## particle discharged whenever idsch.ne.0
    IF(IDSCH.NE.0)KLC(IPART,2)=0

   ENDIF

  ENDDO

  !## points
  IF(PL%ITYPE.EQ.2)CALL GLEND()
  
  CALL GLENDLIST()
 
 ENDDO
 
 PL%TCUR=TTMAX/DYEAR; CALL WDIALOGPUTREAL(IDF_REAL7,PL%TCUR) 
 
 !## clean drawing list, whenever nothing in it
 IF(NPACT.LE.0)THEN
  DO IG=1,NSPG
   CALL GLDELETELISTS(PLLISTINDEX(PL%NPER,IG),1_GLSIZEI); PLLISTINDEX(PL%NPER,IG)=0
  ENDDO
 ENDIF
 
 !## trace as long as there are active particles available
 IF(SUM(PLLISTINDEX).NE.0)TRACE_3D_COMPUTE=.TRUE.
 
 END FUNCTION TRACE_3D_COMPUTE

 !###======================================================================
 SUBROUTINE TRACE_3D_CLOSE()
 !###======================================================================
 IMPLICIT NONE

 !## deallocate memory
 CALL TRACEDEALLOCATE(1)
 IF(ALLOCATED(IVISIT))DEALLOCATE(IVISIT)
 IF(ALLOCATED(LVISIT))DEALLOCATE(LVISIT)
 
 IF(ALLOCATED(SPGCLR))DEALLOCATE(SPGCLR)
 IF(ALLOCATED(SPGPOS))DEALLOCATE(SPGPOS)
 
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
 INTEGER :: I,J,ILAY,ITYPE,IPER,IPART,NCONS,IPERIOD,NIDSCH,IDSCH,IWIN,IP,IRAT,IRAT1,DPER,MPER,SPER
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
  IF(.NOT.TRACEPREPARESP(IBATCH))RETURN
  !## initialize outputfiles
  IF(.NOT.TRACEINITOUTFILES())RETURN
  !#allocate memory to store particles (spec. for transient sim.)
  CALL TRACEALPART()
  !## read particle in memory
  !## set initial time for each particle to zero
  CALL TRACEREADSP()

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
     DO IPART=1,NPART
      IF(KLC(IPART,2).NE.0)I=I+1
     ENDDO
     STRING='Still tracing '//TRIM(ITOS(I))//' out of '//TRIM(ITOS(NPART))//' particles for Stress '// &
                              TRIM(ITOS(IPER))//' out of '//TRIM(ITOS(NPER))//TRIM(CPERIOD)
     IF(IBATCH.EQ.1)WRITE(*,'(A)') TRIM(STRING)
     IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,STRING)
    ELSE
     STRING='Tracing '//TRIM(ITOS(NPART))//' particles (Steady-state) ...'
     IF(IBATCH.EQ.1)WRITE(*,'(A)') TRIM(STRING)
     IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,STRING)
    ENDIF

    NIDSCH=0
    DO IPART=1,NPART
     CALL WMESSAGEPEEK(ITYPE,MESSAGE)
     !## time in days!
     TIME=TOT(IPART,2)
     !## trace selected particle, NOT YET discharged!
     IF(KLC(IPART,2).NE.0)THEN
      CALL FLOLIN(IPART,IMODE(1),TIME,TTMAX,IDSCH,JLC(IPART,2),ILC(IPART,2),KLC(IPART,2),   &
                  XLC(IPART,2),YLC(IPART,2),ZLC(IPART,2),ZLL(IPART,2),IBOUND,ZBOT, &
                  ZTOP,LDELR,LDELC,QX,QY,QZ,QSS,POR,NCON,IDF%NCOL,IDF%NROW,NLAY,  &
                  NLPOR,IDF%NCOL*IDF%NROW*NLAY,NCP1,NRP1,NLP1,ISNK,IREV,FRAC,IMODE(1),   &
                  ISS,MAXVELOCITY,DELX,DELY,MAXILAY(IPART),IVISIT,LVISIT,NVISIT)
      !## clean visited places that have been visited before
      DO I=1,NVISIT; IVISIT(LVISIT(I))=INT(0,1); ENDDO

      !## time in days!
      TOT(IPART,2)=TIME !(days)
      IF(ISS.EQ.1)THEN
       !## end of current simulation
       IF(IDSCH.EQ.7)IDSCH=0
      ELSE
       !## end of simulation reached!
       IF(TOT(IPART,2).GE.TTMAX)IDSCH=7
      ENDIF
      !## particle discharged whenever idsch.ne.0
      IF(IDSCH.NE.0)THEN
       !## write endpoint information current particle that has stopped
       IF(IMODE(2).GT.0)CALL TRACECREATEIPF(IMODE(2),IDSCH,IPART)
       KLC(IPART,2)=0
      ELSE
       !## particle NOT discharged!
       NIDSCH=NIDSCH+1
      ENDIF
     ENDIF
     IF(IBATCH.EQ.0)CALL UTL_WAITMESSAGE(IRAT,IRAT1,IPART,NPART,'Busy tracking particle '//TRIM(ITOS(IPART))//' ')
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
   IF(IPER.EQ.MPER)THEN !NPER)THEN
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
   DO IPART=1,NPART; IF(KLC(IPART,2).NE.0)CALL TRACECREATEIPF(IMODE(2),IDSCH,IPART); ENDDO
  ENDIF

  IF(IMODE(1).GT.0)THEN; CLOSE(IMODE(1)); IMODE(1)=1; ENDIF
  IF(IMODE(2).GT.0)THEN; CLOSE(IMODE(2)); IMODE(2)=1; ENDIF
  
  STRING='Completed particle tracking. '// &
   'Results are stored within: '//CHAR(13)//TRIM(IFFFNAME(ISPFNAME))//CHAR(13)//'and added to the iMOD-manager.'//CHAR(13)//CHAR(13)// &
    TRIM(ITOS(NPART))//' particles were released out of '//TRIM(ITOS(TPART))//'. '//CHAR(13)//  &
   'Unreleased particles occured due to inactive/constant head boundary conditions'//CHAR(13)// &
   'and/or particles positioned above/beneath given thresshold.'//CHAR(13)//&
    TRIM(ITOS(NCONS))//' inconsequences were removed from top/bottom information!'//CHAR(13)//CHAR(13)// &
    'IMPORTANT: Maximum velocity that occured: '//TRIM(RTOS(MAXVELOCITY,'E',4))//' m/day'
  WRITE(IULOG,'(/A/)') TRIM(STRING)
  IF(IBATCH.EQ.1)WRITE(*,'(A)') TRIM(STRING)
  IF(IBATCH.EQ.0)THEN
   CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,TRIM(STRING),'Information')
!   CALL WINDOWOPENCHILD(IWIN,FLAGS=SYSMENUON,WIDTH=1000,HEIGHT=500)
!   CALL WINDOWSELECT(IWIN)
!   CALL WEDITFILE(TRIM(PREFVAL(1))//'\tmp\imodpath.log',ITYPE=MODAL,IDMENU=0, &
!                  IFLAGS=NOTOOLBAR+VIEWONLY+WORDWRAP+NOFILENEWOPEN+NOFILESAVEAS,&
!                  IFONT=4,ISIZE=10)
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
   CALL TRACEINITOUTFILES_IPF(JU(I,2))
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
 LOGICAL FUNCTION TRACEINITOUTFILES()
 !###======================================================================
 IMPLICIT NONE
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
  CALL TRACEINITOUTFILES_IPF(IMODE(2))
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
 SUBROUTINE TRACEINITOUTFILES_IPF(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU

 WRITE(IU,'(I10)') NPART
 WRITE(IU,'(A)') '17'
 WRITE(IU,'(A)') 'SP_XCRD.'
 WRITE(IU,'(A)') 'SP_YCRD.'
 WRITE(IU,'(A)') 'SP_ZCRD.'
 WRITE(IU,'(A)') 'START_ILAY'
 WRITE(IU,'(A)') 'START_IROW'
 WRITE(IU,'(A)') 'START_ICOL'
 WRITE(IU,'(A)') 'EP_XCRD.'
 WRITE(IU,'(A)') 'EP_YCRD.'
 WRITE(IU,'(A)') 'EP_ZCRD.'
 WRITE(IU,'(A)') 'END_ILAY'
 WRITE(IU,'(A)') 'END_IROW'
 WRITE(IU,'(A)') 'END_ICOL'
 WRITE(IU,'(A)') 'IDENT.NO.'
 WRITE(IU,'(A)') 'TIME(YEARS)'
 WRITE(IU,'(A)') 'DISTANCE'
 WRITE(IU,'(A)') 'CAPTURED_BY'
 WRITE(IU,'(A)') 'MAX_ILAY'
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
 READ(IU,*,IOSTAT=IOS) FRAC;  IF(.NOT.TRACECHECKRUN(IOS,'FRAC'))RETURN
 IF(ISNK.NE.2)FRAC=0.99; FRAC=MIN(0.99,MAX(0.0,FRAC))
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

 ISNK=ISNK-1

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
 LOGICAL :: LEX

 IF(ASSOCIATED(XLC))DEALLOCATE(XLC); IF(ASSOCIATED(YLC))DEALLOCATE(YLC)
 IF(ASSOCIATED(ZLC))DEALLOCATE(ZLC); IF(ASSOCIATED(ZLL))DEALLOCATE(ZLL)
 IF(ASSOCIATED(TOT))DEALLOCATE(TOT); IF(ASSOCIATED(KLC))DEALLOCATE(KLC)
 IF(ASSOCIATED(JLC))DEALLOCATE(JLC); IF(ASSOCIATED(ILC))DEALLOCATE(ILC)
 IF(ASSOCIATED(SLAY))DEALLOCATE(SLAY)
 IF(ASSOCIATED(MAXILAY))DEALLOCATE(MAXILAY)

 IF(ICODE.EQ.0)RETURN
! IF(ISPFNAME.NE.NSPFNAME)RETURN

 IF(ALLOCATED(ITBFNAME))DEALLOCATE(ITBFNAME)
 IF(ALLOCATED(HFFNAME))DEALLOCATE(HFFNAME)
 IF(ALLOCATED(XP))DEALLOCATE(XP); IF(ALLOCATED(YP))DEALLOCATE(YP)
 IF(ALLOCATED(X))DEALLOCATE(X); IF(ALLOCATED(Y))DEALLOCATE(Y)
 IF(ALLOCATED(QX))DEALLOCATE(QX); IF(ALLOCATED(QY))DEALLOCATE(QY)
 IF(ALLOCATED(QZ))DEALLOCATE(QZ); IF(ALLOCATED(POR))DEALLOCATE(POR)
 IF(ALLOCATED(DELR))DEALLOCATE(DELR); IF(ALLOCATED(DELC))DEALLOCATE(DELC)
 IF(ALLOCATED(DELX))DEALLOCATE(DELX); IF(ALLOCATED(DELY))DEALLOCATE(DELY)
 IF(ALLOCATED(LDELR))DEALLOCATE(LDELR); IF(ALLOCATED(LDELC))DEALLOCATE(LDELC)
 IF(ALLOCATED(ZBOT))DEALLOCATE(ZBOT); IF(ALLOCATED(ZTOP))DEALLOCATE(ZTOP)
 IF(ALLOCATED(BUFF))DEALLOCATE(BUFF); IF(ALLOCATED(QSS))DEALLOCATE(QSS)
 IF(ALLOCATED(IBOUND))DEALLOCATE(IBOUND); IF(ALLOCATED(NCON))DEALLOCATE(NCON)
 IF(ALLOCATED(PLIPER))DEALLOCATE(PLIPER)
 IF(ALLOCATED(SPFNAME))DEALLOCATE(SPFNAME)
 IF(ALLOCATED(IFFFNAME))DEALLOCATE(IFFFNAME)
 
 INQUIRE(UNIT=IULOG,OPENED=LEX); IF(LEX)CLOSE(IULOG)

 END SUBROUTINE TRACEDEALLOCATE

 !###======================================================================
 SUBROUTINE TRACECREATEIPF(IU,IDSCH,IPART)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDSCH,IPART,IU
 REAL :: DIST
 INTEGER :: IC1,IC2,IR1,IR2
 CHARACTER(LEN=256) :: LINE
 
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
 
 CALL IDFIROWICOL(IDF,IR1,IC1,XLC(IPART,1)+IDF%XMIN,YLC(IPART,1)+IDF%YMIN)
 CALL IDFIROWICOL(IDF,IR2,IC2,XLC(IPART,2)+IDF%XMIN,YLC(IPART,2)+IDF%YMIN)
 
 DIST=SQRT((XLC(IPART,1)-XLC(IPART,2))**2.0+ &
           (YLC(IPART,1)-YLC(IPART,2))**2.0+ &
           (ZLC(IPART,1)-ZLC(IPART,2))**2.0)

 ! LINE=TRIM(ADJUSTL(STRING(I,1)))
 ! DO J=2,NCOLIPF
 !  LINE=TRIM(LINE)//','//TRIM(ADJUSTL(STRING(I,J)))
 ! END DO
 ! WRITE(IU,*,IOSTAT=IOS) TRIM(LINE)!(TRIM((STRING(I,J))//','),J=1,NCOLIPF)

 LINE=TRIM(RTOS(XLC(IPART,1)+IDF%XMIN,'F',2))//','// &
      TRIM(RTOS(YLC(IPART,1)+IDF%YMIN,'F',2))//','// &
      TRIM(RTOS(ZLC(IPART,1),'F',3))//','// &
      TRIM(ITOS(SLAY(IPART)))//','// &      
      TRIM(ITOS(IR1))//','// &      
      TRIM(ITOS(IC1))//','// &      
      TRIM(RTOS(XLC(IPART,2)+IDF%XMIN,'F',2))//','// &
      TRIM(RTOS(YLC(IPART,2)+IDF%YMIN,'F',2))//','// &
      TRIM(RTOS(ZLC(IPART,2),'F',3))//','// &
      TRIM(ITOS(KLC(IPART,2)))//','// &      
      TRIM(ITOS(IR2))//','// &      
      TRIM(ITOS(IC2))//','// &      
      TRIM(ITOS(IPART))//','// &      
      TRIM(RTOS(TOT(IPART,2)/365.25,'E',5))//','// &
      TRIM(RTOS(DIST,'F',2))//','// &
      TRIM(ITOS(IDSCH))//','// &            
      TRIM(ITOS(MAXILAY(IPART)))
 WRITE(IU,'(A)') TRIM(LINE)

! WRITE(IU,'(2(3(E15.8,1X),I10,1X),I10,2(E15.8,1X),2I10)') &
!       XLC(IPART,1)+IDF%XMIN,YLC(IPART,1)+IDF%YMIN,ZLC(IPART,1),SLAY(IPART), &
!       XLC(IPART,2)+IDF%XMIN,YLC(IPART,2)+IDF%YMIN,ZLC(IPART,2),KLC(IPART), &
!       IPART,TOT(IPART,2)/365.25,DIST,IDSCH,MAXILAY(IPART)

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
 ALLOCATE(BUFF(IDF%NCOL,IDF%NROW,NLAY))
 ALLOCATE(QSS(IDF%NCOL,IDF%NROW,NLAY))
 ALLOCATE(IBOUND(IDF%NCOL,IDF%NROW,NLAY))
 ALLOCATE(NCON(NLAY))

 RETURN
 END SUBROUTINE

 !###======================================================================
 SUBROUTINE TRACEALPART()
 !###======================================================================
 IMPLICIT NONE

 !## reals
 ALLOCATE(XLC(NPART,2))
 ALLOCATE(YLC(NPART,2))
 ALLOCATE(ZLC(NPART,2))
 ALLOCATE(ZLL(NPART,2))
 ALLOCATE(TOT(NPART,2))
 !## integers
 ALLOCATE(KLC(NPART,2))
 ALLOCATE(JLC(NPART,2))
 ALLOCATE(ILC(NPART,2))
 ALLOCATE(SLAY(NPART))
 ALLOCATE(MAXILAY(NPART))
 
 END SUBROUTINE

 !###======================================================================
 LOGICAL FUNCTION TRACEREADBUDGET(IPER,IBATCH)
 !###======================================================================
 IMPLICIT NONE
 REAL,PARAMETER :: QCRIT=0.1 !10.0  !GROTER DAN DIT, POTENTIAL DISCARGE CELLS
 INTEGER,INTENT(IN) :: IBATCH,IPER
 CHARACTER(LEN=256) :: STRING
 INTEGER :: ILAY,IU,IROW,ICOL,ITYPE
 REAL :: QERROR,NODATAVALUE
 TYPE(WIN_MESSAGE) :: MESSAGE

 TRACEREADBUDGET=.FALSE.

 !## read flux-right-face
 BUFF=0.0
 QX  =0.0
 DO ILAY=1,NLAY
  CALL WMESSAGEPEEK(ITYPE,MESSAGE)
  STRING='Reading BDGFRF L'//TRIM(ITOS(ILAY))//' '//TRIM(HFFNAME(1,ILAY,IPER))//'...'
  IF(IBATCH.EQ.1)WRITE(*,'(A)') TRIM(STRING)
  IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,STRING)
  CALL TRACEREADBLOCK_R(BUFF(1,1,ILAY),IDF%NROW,IDF%NCOL,HFFNAME(1,ILAY,IPER),IU,4,DELC(0),DELR(0),0,QSS,NODATAVALUE)
  IF(IU.LE.0)RETURN
  DO IROW=1,IDF%NROW; DO ICOL=1,IDF%NCOL
   QX(ICOL+1,IROW,ILAY)=-BUFF(ICOL,IROW,ILAY)
  ENDDO; ENDDO
!  idf%x=buff(:,:,ilay)
!  IF(.not.idfwrite(idf,TRIM(PREFVAL(1))//'\qx_l'//TRIM(ITOS(ILAY))//'.idf',1))then
!  endif
 ENDDO

 !## read flux-front-face
 BUFF=0.0
 QY  =0.0
 DO ILAY=1,NLAY
  CALL WMESSAGEPEEK(ITYPE,MESSAGE)
  STRING='Reading BDGFFF L'//TRIM(ITOS(ILAY))//' '//TRIM(HFFNAME(2,ILAY,IPER))//'...'
  IF(IBATCH.EQ.1)WRITE(*,'(A)') TRIM(STRING)
  IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,STRING)
  CALL TRACEREADBLOCK_R(BUFF(1,1,ILAY),IDF%NROW,IDF%NCOL,HFFNAME(2,ILAY,IPER),IU,4,DELC(0),DELR(0),0,QSS,NODATAVALUE)
  IF(IU.LE.0)RETURN
  DO ICOL=1,IDF%NCOL; DO IROW=1,IDF%NROW
   QY(ICOL,IROW+1,ILAY)= BUFF(ICOL,IROW,ILAY)
  ENDDO; ENDDO
!  idf%x=buff(:,:,ilay)
!  IF(.not.idfwrite(idf,TRIM(PREFVAL(1))//'\qy_l'//TRIM(ITOS(ILAY))//'.idf',1))then
!  endif
 ENDDO

 !## read flux-lower-face
 QZ  =0.0
 IF(NLAY.GT.1)THEN
  BUFF=0.0
  DO ILAY=1,NLAY-1
   CALL WMESSAGEPEEK(ITYPE,MESSAGE)
   STRING='Reading BDGFLF L'//TRIM(ITOS(ILAY))//' '//TRIM(HFFNAME(3,ILAY,IPER))//'...'
   IF(IBATCH.EQ.1)WRITE(*,'(A)') TRIM(STRING)
   IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,STRING)
   CALL TRACEREADBLOCK_R(BUFF(1,1,ILAY),IDF%NROW,IDF%NCOL,HFFNAME(3,ILAY,IPER),IU,4,DELC(0),DELR(0),0,QSS,NODATAVALUE)
   IF(IU.LE.0)RETURN
!   idf%x=buff(:,:,ilay)
!   IF(.not.idfwrite(idf,TRIM(PREFVAL(1))//'\qz_l'//TRIM(ITOS(ILAY))//'.idf',1))then
!   endif
  ENDDO
  DO IROW=1,IDF%NROW; DO ICOL=1,IDF%NCOL
   DO ILAY=1,NLAY
    QZ(ICOL,IROW,ILAY+1)= BUFF(ICOL,IROW,ILAY) !standaard modflow does it wrong!
   ENDDO
  ENDDO; ENDDO
 ENDIF

 !##initialize qss - as nett-term waterbalance! - wordt alleen gebruikt in combi. met frac!!!
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
 !##oorspronkelijk code flags all buff().lt.0 ! - NOODZAKELIJK OM PARTICLES TE LATEN STOPPEN!!!!
    IF(QSS(ICOL,IROW,ILAY).LT.-QCRIT.AND.IBOUND(ICOL,IROW,ILAY).NE.0)IBOUND(ICOL,IROW,ILAY)=-2000
    IF(QSS(ICOL,IROW,ILAY).GT. QCRIT.AND.IBOUND(ICOL,IROW,ILAY).NE.0)IBOUND(ICOL,IROW,ILAY)= 2000
 !   IF(QSS(ICOL,IROW,ILAY).LT.QCRIT.AND.ABS(IBOUND(ICOL,IROW,ILAY)).LT.1000)IBOUND(ICOL,IROW,ILAY)=1000*IBOUND(ICOL,IROW,ILAY)
   ENDDO
  ENDDO
!  idf%x=qss(:,:,ilay)
!  IF(.not.idfwrite(idf,TRIM(PREFVAL(1))//'\qerror_l'//TRIM(ITOS(ILAY))//'.idf',1))then
!  endif
 ENDDO
! deallocate(idf%x)
 
! CS=DELR(1)-DELR(0)
! DO ILAY=1,NLAY
!  CALL WRT2IDF(TRIM(PREFVAL(1))//'\tmp\qx_l'//TRIM(itos(ilay))//'.IDF',qx(1,1,ilay),IDF%NCol,IDF%NROW,0.0,cs,delr(0),delc(0))
!  CALL WRT2IDF(TRIM(PREFVAL(1))//'\tmp\qy_l'//TRIM(itos(ilay))//'.IDF',qy(1,1,ilay),IDF%NCOL,IDF%NRow,0.0,cs,delr(0),delc(0))
!  CALL WRT2IDF(TRIM(PREFVAL(1))//'\tmp\qerror_l'//TRIM(itos(ilay))//'.IDF',qss(1,1,ilay),IDF%NCOL,IDF%NROW,0.0,cs,delr(0),delc(0))
!  CALL WRT2IDF('qerror_l'//TRIM(itos(ilay))//'.IDF',qss(1,1,ilay),IDF%NCOL,IDF%NROW,0.0,cs,delr(0),delc(0))
! END DO

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
    CALL TRACEREADBLOCK_I(IBOUND(1,1,ILAY),IDF%NROW,IDF%NCOL,ITBFNAME(1,ILAY),IU,1,DELC,DELR)
    IF(IU.LE.0)RETURN
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
   !## non-smoothing in case of startingpoints will be not smoothed!
   CALL TRACEREADBLOCK_R(ZTOP(1,1,ILAY),IDF%NROW,IDF%NCOL,ITBFNAME(2,ILAY),IU,2,DELC,DELR,0,BUFF,NODATAVALUE)
   IF(IU.LE.0)RETURN
   !## adjust boundary whenever top
   DO IROW=1,IDF%NROW; DO ICOL=1,IDF%NCOL
    IF(ZTOP(ICOL,IROW,ILAY).EQ.NODATAVALUE)IBOUND(ICOL,IROW,ILAY)=0
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
   !## non-smoothing in case of startingpoints will be not smoothed!
   CALL TRACEREADBLOCK_R(ZBOT(1,1,ILAY),IDF%NROW,IDF%NCOL,ITBFNAME(3,ILAY),IU,2,DELC,DELR,0,BUFF,NODATAVALUE)
   IF(IU.LE.0)RETURN
   !## adjust boundary whenever top
   DO IROW=1,IDF%NROW; DO ICOL=1,IDF%NCOL
    IF(ZBOT(ICOL,IROW,ILAY).EQ.NODATAVALUE)IBOUND(ICOL,IROW,ILAY)=0
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
   CALL TRACEREADBLOCK_R(POR(1,1,ILAY),IDF%NROW,IDF%NCOL,ITBFNAME(4,ILAY),IU,2,DELC,DELR,1,BUFF,NODATAVALUE)
   IF(IU.LE.0)RETURN
   !## adjust boundary whenever por
   DO IROW=1,IDF%NROW; DO ICOL=1,IDF%NCOL
    IF(POR(ICOL,IROW,ILAY).EQ.NODATAVALUE)IBOUND(ICOL,IROW,ILAY)=0
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
   CALL TRACEREADBLOCK_R(POR(1,1,NLAY+ILAY),IDF%NROW,IDF%NCOL,ITBFNAME(5,ILAY),IU,2,DELC,DELR,1,BUFF,NODATAVALUE)
   IF(IU.LE.0)RETURN
   !## adjust boundary whenever por
   DO IROW=1,IDF%NROW; DO ICOL=1,IDF%NCOL
    IF(POR(ICOL,IROW,NLAY+ILAY).EQ.NODATAVALUE)THEN; IBOUND(ICOL,IROW,ILAY)=0; IBOUND(ICOL,IROW,ILAY+1)=0;ENDIF
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
  DELR(0)=0.0 !IDF%XMIN
  DO I=1,IDF%NCOL
   DELR(I)=REAL(I)*IDF%DX
!   DELR(I)=DELR(0)+REAL(I)*IDF%DX
!   DELR(I)=DELR(I-1)+IDF%DX
  END DO
!  DO I=1,IDF%NCOL
  DO I=0,IDF%NCOL
   DELR(I)=IDF%XMIN+DELR(I)
  ENDDO
  
  DELC(0)=0.0 !IDF%YMAX
  DO I=1,IDF%NROW
   DELC(I)=-REAL(I)*IDF%DY
!   DELC(I)=DELC(0)-REAL(I)*IDF%DY
!   write(*,*) delc(i),delc(0),real(i)*idf%dy
!   DELC(I)=DELC(I-1)-IDF%DY
  END DO
!  DO I=1,IDF%NROW
  DO I=0,IDF%NROW
   DELC(I)=IDF%YMAX+DELC(I)
  ENDDO
  
  !#fill local coordinates
!  LDELR(1)=IDF%DX
  DO I=1,IDF%NCOL
   LDELR(I)=REAL(I)*IDF%DX
!   LDELR(I)=LDELR(I-1)+IDF%DX
  END DO
!  LDELC(IDF%NROW)=IDF%DY
  II=0
  DO I=IDF%NROW,1,-1 
   II=II+1
   LDELC(I)=REAL(II)*IDF%DY
  ENDDO
!  DO I=2,IDF%NROW
!   II=IDF%NROW+1-I
!   LDELC(II)=LDELC(II+1)+IDF%DY
!  ENDDO

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
 
! write(*,*) idf%xmin,idf%xmax
! do i=1,idf%ncol
!  write(*,*) i,delr(i),ldelr(i)
! enddo
! write(*,*) idf%xmin,idf%xmax
! write(*,*) idf%ymin,idf%ymax
! do i=1,idf%nrow
!  write(*,*) i,delc(i),ldelc(i)
! enddo
! write(*,*) idf%ymin,idf%ymax
 
 END SUBROUTINE

END MODULE MOD_PLINES_TRACE

