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
!!  iMOD is partly based on the USGS MODPATH source code;
!!  for iMOD the USGS MODPATH source code has been expanded
!!  and extensively modified by Stichting Deltares.
!!  The original USGS MODPATH source code can be downloaded from the USGS 
!!  website http://www.usgs.gov/. The original USGS MODPATH code incorporated 
!!  in this file is covered by the USGS Software User Rights Notice;
!!  you should have received a copy of this notice along with this program. 
!!  If not, see <http://water.usgs.gov/software/help/notice/>.
!!
MODULE MOD_PLINES_FLOLIN

USE WINTERACTER
USE OPENGL
USE MOD_UTL, ONLY : ITOS,RTOS
USE MOD_PLINES_PAR, ONLY : IDF
USE MOD_3D_PAR, ONLY : MIDPOS,TOP,BOT,XYZAXES

REAL,PARAMETER :: ACCURACY=0.0 !EPSILON(1.0)
REAL,PARAMETER :: PLUSINFINITY=HUGE(1.0)
REAL,PARAMETER :: MINUSINFINITY=TINY(1.0)

CONTAINS

 !###====================================================================
 SUBROUTINE FLOLIN(IPART,MODE,TIME,TMAX,IDSCH,JP,IP,KP, &
  XP,YP,ZP,ZLOC,IBOUND,ZBOT,ZTOP,XMAX,YMAX,QX,QY,QZ,QSS,&
  POR,NCON,NCOL,NROW,NLAY,NLPOR,NZDIM,NCP1,NRP1,NLP1,ISNK,&
  IREV,FRAC,I2,ISS,MAXVELOCITY,DELX,DELY,MAXILAY,IVISIT,LVISIT,NVISIT)
 !###====================================================================
 !  THIS SUBROUTINE COMPUTES THE PATH OF A PARTICLE FROM AN INITIAL
 !  STARTING LOCATION AND STARTING TIME TO A DISCHARGE POINT OR A
 !  SPECIFIED POINT IN TIME.  IF THE LINE MODE IS USED (MODE=1), THE
 !  COORDINATES OF EACH COMPUTED POINT ARE WRITTEN TO A FILE NAMED
 !  "PATHLINE"
 ! CHANGED PARAMETER: IDSCH
 ! IDSCH=-1 ERROR OCCURED
 ! IDSCH= 0 INITIAL VALUE
 ! IDSCH= 1 INACTIVE CELL
 ! IDSCH= 2 VELOCITY=0.0
 ! IDSCH= 3 NO OUTFLOW
 ! IDSCH= 4 WEAK SINK NOT CONCERNING FLUX
 ! IDSCH= 5 WEAK SINK F GT FRAC
 ! IDSCH= 6 BOUNDARY GRID REACHED
 ! IDSCH= 7 T GT TMAX
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NROW,NCOL,NLAY,NZDIM,NCP1,NLP1,NRP1,NLPOR,IREV,ISS, &
                       ISNK,MODE,IPART,I2
 INTEGER,INTENT(IN OUT) :: JP,IP,KP,MAXILAY
 REAL,INTENT(IN) :: TMAX,FRAC
 REAL,INTENT(IN OUT) :: TIME,ZLOC,XP,YP,ZP,MAXVELOCITY
 INTEGER,INTENT(OUT) :: IDSCH,NVISIT
 INTEGER,DIMENSION(NCOL,NROW,NLAY),INTENT(IN) :: IBOUND
 INTEGER,DIMENSION(NLAY),INTENT(IN) :: NCON
 INTEGER(KIND=1),DIMENSION(NCOL*NROW*NLAY),INTENT(INOUT) :: IVISIT
 INTEGER(KIND=4),DIMENSION(NCOL*NROW*NLAY),INTENT(INOUT) :: LVISIT
 REAL,DIMENSION(NZDIM),INTENT(IN) :: ZTOP,ZBOT
 REAL,DIMENSION(NCOL,NROW,NLAY),INTENT(IN) :: QSS
 REAL,DIMENSION(NCOL,NRP1,NLAY),INTENT(IN) :: QY
 REAL,DIMENSION(NCP1,NROW,NLAY),INTENT(IN) :: QX
 REAL,DIMENSION(NCOL,NROW,NLP1),INTENT(IN) :: QZ
 REAL,DIMENSION(NCOL,NROW,NLPOR),INTENT(IN) :: POR
 REAL,DIMENSION(NCOL),INTENT(IN) :: XMAX,DELX
 REAL,DIMENSION(NROW),INTENT(IN) :: YMAX,DELY

 INTEGER                           :: KOLD,IBND,KPOR,NK,NKP,ZL,  &
                                      LCON,NSEGS,N,JOLD,IOLD,    &
                                      IX,IY,IZ,IVXFLG,IVYFLG,    &
                                      IVZFLG,VSIGN,IXYZ,IEXIT,ICB
 REAL                              :: DT,TIM,ZMX,ZCB,ZMN,DTT,V, &
                                      ZLC,XMN,XMX,YMN,YMX,VX1,VX2,  &
                                      VY1,VY2,VZ1,VZ2,VX,VY,VZ,DTX, &
                                      DTY,DZ,PHI,DVXDX,DVYDY,DVZDZ, &
                                      DXDY,DXDZ,DYDZ,QINOUT,DTZ,QI, &
                                      VLX1,VLX2,VLY1,VLY2,VLZ1,     &
                                      VLZ2,F,XPP,YPP,ZPP,XNEW,YNEW, &
                                      ZNEW,VELOCITY,TTM!,MAXVELOCITY

 LOGICAL                           :: LOGCON  !in confining bed

 IF(ZLOC.LE.0.0)ZLOC=0.001
 IF(ZLOC.GE.1.0)ZLOC=0.999
 
 KOLD =KP
 IDSCH=0
 IBND =IBOUND(JP,IP,KP)

 !#####return if particle is in inactive cell
 IF(IBND.EQ.0)THEN 
  IDSCH=1
  RETURN
 ENDIF

 !#####initialize variables and compute initial value of
 !#####normalized z coordinate

 !#####current nodenumber
 NK =(KP-1)*NCOL*NROW+(IP-1)*NCOL+JP
 !#####nodenumber underneath
 NKP=NK+(NCOL*NROW)

 ZMX=ZTOP(NK)
 IF(ZLOC.GE.0.0)THEN
  ZP=ZLOC*ZMX+(1.0-ZLOC)*ZBOT(NK)
 ELSE
  ZL=1.0+ZLOC
  ZP=ZL*ZBOT(NK)+(1.0-ZL)*ZTOP(NKP)
 ENDIF
 !#####write starting coordinates
 IF(MODE.GT.0)THEN
  CALL WRITERESULT(I2,IPART,KP,XP,YP,ZP,TIME/365.25,0.0,IP,JP)
 ENDIF

 !#####particle in confining bed
 LOGCON=.TRUE.
 IF(ZLOC.GT.0.0)LOGCON=.FALSE.
 IF(ZLOC.GE.0.0.AND.QZ(JP,IP,KP+1).GE.0.0)LOGCON=.FALSE.
 IF(LOGCON)THEN

  LCON=NCON(KP)
  KPOR=NLAY+LCON

 !######vertical velocity
  V=QZ(JP,IP,KP+1)/POR(JP,IP,KPOR)/DELY(IP)/DELX(JP)
  IF(V.EQ.0.0)THEN
   IDSCH=2
   RETURN
  ENDIF

 !######determine elapsed time by current velocity
  ZCB=ZTOP(NKP)
  ZMN=ZBOT(NK)
  IF(V.GT.0.0)DT=-ZLOC*(ZMN-ZCB)/V
  IF(V.LT.0.0)DT=-(1.0+ZLOC)*(ZMN-ZCB)/V
  TIM=TIME+DT

 !######check to see if maximum time has been reached or exceeded.
 !######if so, set time equal to tmax, caluculate particle location,
 !######exit subroutine.
  IF(TMAX.GT.TIM)THEN ! GO TO 10                                          # C01060
  ELSE !IF(TIM.GE.TMAX)THEN
   DTT= TMAX-TIME
   ZLC= ZLOC+V*DTT/(ZMN-ZCB)
   ZP =-ZLC*ZCB+(1.0+ZLC)*ZMN
   TTM= TMAX                                                          !   # C01100
   IF(MODE.GT.0)THEN
    CALL WRITERESULT(I2,IPART,KP,XP,YP,ZP,TTM/365.25,V,IP,JP)
   ENDIF
   TIME =TMAX
   ZLOC =ZLC
   IDSCH=7
   RETURN
  ENDIF

  TIME=TIME+DT
  ZP  =ZMN
  ZLOC=0.0
 !######downwards velocity increase current layer
  IF(V.GE.0.0)THEN ! GO TO 20                                             # C01210
  ELSE !IF(V.LT.0.0)THEN
   KP  =KP+1
   ZP  =ZCB
   ZLOC=1.0
  ENDIF
  IF(MODE.GT.0)THEN
   CALL WRITERESULT(I2,IPART,KP,XP,YP,ZP,TIME/365.25,V,IP,JP)
  ENDIF
 !#####the particle is now in an active model layer.  check to see if it is
 !#####in an inactive cell within the model layer.  if so, reset
 !#####kp and zloc to the values at the exit point of previous active cell.
  IBND=IBOUND(JP,IP,KP)
  IF(IBND.EQ.0)THEN 
   IDSCH= 1
   KP   = KOLD
   ZLOC =-1.0
   RETURN
  ENDIF

 ENDIF

 !#####start tracking loop
 NSEGS=NCOL*NROW*NLAY
 DO N=1,NSEGS

  !######store current cell location and local z coordinate
  JOLD =JP
  IOLD =IP
  KOLD =KP
  
  NK=(KP-1)*NCOL*NROW+(IP-1)*NCOL+JP

  IVISIT(NK)=IVISIT(NK)+INT(1,1)
  LVISIT(N) =NK
  NVISIT=N
  IF(IVISIT(NK).GT.INT(10,1))EXIT
    
 !######assign scalar variables for convenience
  XMN=0.0
  IF(JP.GT.1)XMN=XMAX(JP-1)
  XMX=XMAX(JP)
  YMN=0.0
  IF(IP.LT.NROW)YMN=YMAX(IP+1)
  YMX=YMAX(IP)

!  NK=(KP-1)*NCOL*NROW+(IP-1)*NCOL+JP

  ZMX=ZTOP(NK)
  ZMN=ZBOT(NK)
  IF(ZMX.EQ.ZMN)ZMX=ZMX+0.01
  ZP=ZLOC*ZMX+(1.0-ZLOC)*ZMN
  DZ=ZMX-ZMN

  !## assign face velocities
  PHI=POR(JP,IP,KP)
  VX1=QX(JP,IP,KP)/PHI/DELY(IP)/DZ
  VX2=QX(JP+1,IP,KP)/PHI/DELY(IP)/DZ
  VY1=QY(JP,IP+1,KP)/PHI/DELX(JP)/DZ
  VY2=QY(JP,IP,KP)/PHI/DELX(JP)/DZ
  VZ1=QZ(JP,IP,KP+1)/PHI/DELY(IP)/DELX(JP)
  VZ2=QZ(JP,IP,KP)/PHI/DELY(IP)/DELX(JP)

  !## compute cell transit times in x, y, and z directions
  CALL DTCALC(VX1,VX2,VX,DVXDX,XMN,XMX,XP,DTX,IVXFLG)
  CALL DTCALC(VY1,VY2,VY,DVYDY,YMN,YMX,YP,DTY,IVYFLG)
  CALL DTCALC(VZ1,VZ2,VZ,DVZDZ,ZMN,ZMX,ZP,DTZ,IVZFLG)

  VELOCITY   =SQRT(VX**2.0+VY**2.0+VZ**2.0)
  MAXVELOCITY=MAX(MAXVELOCITY,VELOCITY)

 !######determine exit face and set flags
  IX=0
  IY=0
  IZ=0
  DT=DTX
  IX=1
  IF(DTY.GE.DT)THEN
  ELSE !IF(DTY.LT.DT)THEN
   DT=DTY
   IX=0
   IY=1
   IZ=0
  ENDIF
  IF(DTZ.GE.DT)THEN
  ELSE !IF(DTZ.LT.DT)THEN
   DT=DTZ
   IY=0
   IX=0
   IZ=1
  ENDIF
  IF(VX.LT.0.0.AND.IX.GT.0)IX=-IX
  IF(VY.LT.0.0.AND.IY.GT.0)IY=-IY
  IF(VZ.LT.0.0.AND.IZ.GT.0)IZ=-IZ

 !######check to see if there is no outflow from this cell.
 !######if simulation is steady state, discharge particle and return.
  IXYZ=0
  IF(IVXFLG.GT.1)IXYZ=IXYZ+1
  IF(IVYFLG.GT.1)IXYZ=IXYZ+1
  IF(IVZFLG.GT.1)IXYZ=IXYZ+1

  IF(IXYZ.EQ.3)THEN !.AND.ISS.EQ.1)THEN
   !## write centre location of dischage point
   IF(MODE.GT.0)THEN
    XP= SUM(DELX(1:JP))   -0.5*DELX(JP)
    YP= SUM(DELY(IP:NROW))-0.5*DELY(IP)
    ZP=(ZMX+ZMN)/2.0
    CALL WRITERESULT(I2,IPART,KP,XP,YP,ZP,TIME/365.25,0.0,IP,JP)
   ENDIF
   IDSCH=3
   RETURN
  ENDIF

 !######check to see if this cell is a discharge point
  IF(IBOUND(JP,IP,KP).LE.-1000.OR.IBOUND(JP,IP,KP).GE.1000)THEN
   IF(ISNK.GE.1)THEN

    IF(TIME.LE.0.0.AND.IREV.EQ.1)THEN !## backwards
!####something to do with weak-sinks too

    ELSE !IF(TIME.GT.0.0.AND.IREV.EQ.0)THEN
 !#########weaksinks discharge particles - not concerning flux
     IF(ISNK.EQ.1)THEN
      IDSCH=4
      RETURN
     ENDIF
    ENDIF
   ENDIF

 !#######do a second check to see if particle cannot get out of cell.
 !#######if it cannot, discharge it.
   IF(TIME.LE.0.0.AND.IREV.EQ.1)THEN  !## backwards
   ELSE !IF(TIME.GT.0.0.AND.IREV.EQ.0)THEN  !forwards???
    DXDY=DELX(JP)*DELY(IP)
    DXDZ=DELX(JP)*(ZMX-ZMN)
    DYDZ=DELY(IP)*(ZMX-ZMN)
    VSIGN=1.0
    IF(IREV.EQ.1)VSIGN= -1.0  !## backwards
    VLX1=VSIGN*VX1
    VLX2=VSIGN*VX2
    VLY1=VSIGN*VY1
    VLY2=VSIGN*VY2
    VLZ1=VSIGN*VZ1
    VLZ2=VSIGN*VZ2
 !########make infiltration also as a weak sink!
    QINOUT= -VSIGN*QSS(JP,IP,KP)

    IF(QINOUT.GT.0.0)THEN
     QI=0.0
     IF(VLX1.GT.0.0)QI=QI+VLX1*DYDZ
     IF(VLX2.LT.0.0)QI=QI-VLX2*DYDZ
     IF(VLY1.GT.0.0)QI=QI+VLY1*DXDZ
     IF(VLY2.LT.0.0)QI=QI-VLY2*DXDZ
     IF(VLZ1.GT.0.0)QI=QI+VLZ1*DXDY
     IF(VLZ2.LT.0.0)QI=QI-VLZ2*DXDY

     IF(QI.GT.0.0)THEN
      F=QINOUT/QI/POR(JP,IP,KP)

      IF(F.GT.FRAC)THEN
       IDSCH=5
       RETURN
      ENDIF
     ENDIF
    ENDIF
   ENDIF
  ENDIF

 !######check to see if time is greater than or equal to tmax.
 !######if so, set time equal to tmax, calculate location & return
  TIM=TIME+DT
  IF(TMAX.GT.TIM)THEN
  ELSE !IF(TMAX.LE.TIM)THEN
   DTT=TMAX-TIME
   CALL NEWXYZ(VX,DVXDX,VX1,VX2,DTT,XP,XMN,XMX,XPP,IVXFLG,0)
   CALL NEWXYZ(VY,DVYDY,VY1,VY2,DTT,YP,YMN,YMX,YPP,IVYFLG,0)
   CALL NEWXYZ(VZ,DVZDZ,VZ1,VZ2,DTT,ZP,ZMN,ZMX,ZPP,IVZFLG,0)

   ZLOC=(ZPP-ZMN)/(ZMX-ZMN)
   TTM=  TMAX
   
 !#######write coordinates of intermediate points
   IF(MODE.GT.0)THEN
    CALL WRITERESULT(I2,IPART,KP,XPP,YPP,ZPP,TTM/365.25,VELOCITY,IP,JP)
   ENDIF
   XP   =XPP
   YP   =YPP
   ZP   =ZPP
   TIME =TMAX
   RETURN
  ENDIF

 !######compute particle coordinates at the exit point of cell (jp,ip,kp)
  CALL NEWXYZ(VX,DVXDX,VX1,VX2,DT,XP,XMN,XMX,XNEW,IVXFLG,IX)
  CALL NEWXYZ(VY,DVYDY,VY1,VY2,DT,YP,YMN,YMX,YNEW,IVYFLG,IY)
  CALL NEWXYZ(VZ,DVZDZ,VZ1,VZ2,DT,ZP,ZMN,ZMX,ZNEW,IVZFLG,IZ)
  XP=XNEW
  YP=YNEW
  ZP=ZNEW
  TIME=TIME+DT

  ZLOC=(ZP-ZMN)/(ZMX-ZMN)
  IF(ZLOC.GT.1.0)ZLOC=1.0
  IF(ZLOC.LT.0.0)ZLOC=0.0

  IF(IX.LT.0)JP=JP-1
  IF(IX.GT.0)JP=JP+1
  IF(IY.LT.0)IP=IP+1
  IF(IY.GT.0)IP=IP-1
  ICB=0

  IF(IZ.LT.0.AND.NCON(KP).NE.0)ICB=1
  IF(IZ.LT.0.AND.NCON(KP).EQ.0)KP=KP+1
  IF(IZ.GT.0)KP=KP-1

  !## store minimal modellayers occurence
  MAXILAY=MAX(MAXILAY,KP)

  !######discharge particle if it reaches boundary of the active grid
  IEXIT=0
  IF(JP.LT.1.OR.JP.GT.NCOL)IEXIT=1
  IF(IP.LT.1.OR.IP.GT.NROW)IEXIT=1
  IF(KP.LT.1.OR.KP.GT.NLAY)IEXIT=1
  IF(IEXIT.EQ.1)THEN
  ELSE !IF(IEXIT.NE.1)THEN
   IBND=IBOUND(JP,IP,KP)
   IF(IBND.EQ.0)IEXIT=1 
  ENDIF

  !## boundary of active grid reached
  IF(IEXIT.EQ.1)THEN
   IDSCH=6
   JP=JOLD
   IP=IOLD
   KP=KOLD
   IF(MODE.GT.0)THEN
    CALL WRITERESULT(I2,IPART,KP,XP,YP,ZP,TIME/365.25,VELOCITY,IP,JP)
   ENDIF
   RETURN
  ENDIF

 !######switch zloc from 0 to 1 or 1 to 0 if particle changes layers
  IF(IZ.LT.0.AND.NCON(KOLD).EQ.0)ZLOC=1.0
  IF(IZ.GT.0.AND.NCON(KP).EQ.0)ZLOC=0.0
  IF(IZ.GT.0.AND.NCON(KP).GT.0)THEN
   ZLOC=-1.0
   ICB = 1
  ENDIF
 !######write coordinates of exit point
  IF(MODE.GT.0)THEN
   CALL WRITERESULT(I2,IPART,KP,XP,YP,ZP,TIME/365.25,VELOCITY,IP,JP)
  ENDIF

 !######check to see if particle has entered confining bed.
 !######if so, move particle through confining bed to an active model layer.
 !######otherwise, go to end of cell loop.
  IF(ICB.EQ.0)THEN
  ELSE !IF(ICB.NE.0)THEN
   KOLD=KP
   LCON=NCON(KP)
   KPOR=NLAY+LCON
   V=QZ(JP,IP,KP+1)/POR(JP,IP,KPOR)/DELY(IP)/DELX(JP)
   IF(V.EQ.0.0)THEN
    IDSCH=2
    RETURN
   ENDIF

   NK=(KP-1)*NCOL*NROW+(IP-1)*NCOL+JP
   NKP=NK+(NCOL*NROW)

   ZCB= ZTOP(NKP)
   ZMN= ZBOT(NK)
   IF(V.GT.0.0)DT=-ZLOC*(ZMN-ZCB)/V
   IF(V.LT.0.0)DT=-(1.0+ZLOC)*(ZMN-ZCB)/V

 !#######check to see if time is greater than or equal to tmax.
 !#######if so, set time equal to tmax, calculate location, &
 !#######exit subroutine.
   TIM=TIME+DT
   IF(TMAX.GT.TIM)THEN
   ELSE !IF(TMAX.LE.TIM)THEN
    DTT= TMAX-TIME
    ZLC= ZLOC+V*DTT/(ZMN-ZCB)
    ZP =-ZLC*ZCB+(1.0+ZLC)*ZMN
    TTM=TMAX
    IF(MODE.GT.0)THEN
     CALL WRITERESULT(I2,IPART,KP,XP,YP,ZP,TTM/365.25,VELOCITY,IP,JP)
    ENDIF
    TIME =TMAX
    ZLOC =ZLC
    IDSCH=7
    RETURN
   ENDIF

   TIME=TIME+DT
   ZP=ZMN
   ZLOC=0.0
   IF(V.GE.0.0)THEN
   ELSE !IF(V.LT.0.0)THEN
    KP  =KP+1
    ZP  =ZCB
    ZLOC=1.0
   ENDIF

   IF(MODE.GT.0)THEN
    CALL WRITERESULT(I2,IPART,KP,XP,YP,ZP,TIME/365.25,VELOCITY,IP,JP)
   ENDIF
   !####### the particle is now in an active model layer.  check to see if it is
   !####### in an inactive cell within the model layer.  if so, reset
   !####### kp and zloc to the values at the exit point of previous active cell.
   IBND=IBOUND(JP,IP,KP)
   IF(IBND.EQ.0)THEN !.OR.HED.GT.1.0E+29)THEN
    IDSCH= 1
    KP   = KOLD
    ZLOC = -1.0
    RETURN
   ENDIF

  ENDIF
 ENDDO
 
 !## error occured -- too long traced, for example
 IDSCH=-1

 END SUBROUTINE FLOLIN

 !###====================================================================
 SUBROUTINE WRITERESULT(IU,IPART,KP,XP,YP,ZP,TIME,V,IROW,ICOL)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPART,KP,IU,IROW,ICOL
 REAL,INTENT(IN) :: XP,YP,ZP,TIME,V
 CHARACTER(LEN=256) :: LINE
 REAL(KIND=GLFLOAT) :: X,Y,Z,DX,DY
 
 IF(.TRUE.)THEN
  LINE=TRIM(ITOS(IPART))//','//TRIM(ITOS(KP))//','//TRIM(RTOS(XP+IDF%XMIN,'F',2))//','// &
       TRIM(RTOS(YP+IDF%YMIN,'F',2))//','//TRIM(RTOS(ZP,'F',3))//','//TRIM(RTOS(TIME,'E',5))//','// &
       TRIM(RTOS(V,'E',5))//','//TRIM(ITOS(IROW))//','//TRIM(ITOS(ICOL))
  WRITE(IU,*) TRIM(LINE)
 ELSE
 
  DX=(TOP%X-BOT%X)/2.0_GLFLOAT/XYZAXES(1)
  DY=(TOP%Y-BOT%Y)/2.0_GLFLOAT/XYZAXES(2)

  !## translate current position to view=position
  X= XP+IDF%XMIN;    Y= YP+IDF%YMIN; Z=ZP 
  X=(X-MIDPOS%X)/DX; Y=(Y-MIDPOS%Y)/DY
  !## current position 
  CALL GLVERTEX3F(X,Y,Z); 
 
 ENDIF
 
 END SUBROUTINE

 !###====================================================================
 SUBROUTINE NEWXYZ(V,DVDX,V1,V2,DT,XP,XMIN,XMAX,XNEW,IVFLG,ISIDE)
 !###====================================================================
 !C  THIS SUBROUTINE COMPUTES A NEW X, Y, OR Z PARTICLE COORDINATE GIVEN
 !C  FACE VELOCITIES AND A TIME INTERVAL.
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IVFLG,ISIDE
 REAL,INTENT(IN)    :: V,DVDX,V1,V2,DT,XP,XMIN,XMAX
 REAL,INTENT(OUT)   :: XNEW

 IF(IVFLG.EQ.1)THEN
  XNEW=XP+V1*DT
 ELSEIF(IVFLG.EQ.2)THEN
  XNEW=XP
 ELSE
  XNEW=XP+V*(EXP(DVDX*DT)-1.0E+0)/DVDX
 ENDIF
 IF(IVFLG.NE.2)THEN
  IF(ISIDE.GT.0)XNEW=XMAX
  IF(ISIDE.LT.0)XNEW=XMIN
 ENDIF

 END SUBROUTINE NEWXYZ

 !###====================================================================
 SUBROUTINE DTCALC(V1,V2,V,DVDX,X1,X2,XP,DT,IVFLG)
 !###====================================================================
 !  THIS SUBROUTINE CALCULATES THE TRANSIT TIME (DT) FROM THE CURRENT
 !  LOCATION TO A POTENTIAL DISCHARGE FACE IN A GIVEN DIRECTION.
 !  IT ALSO DETERMINES IF THERE IS NO POTENTIAL FOR DISCHARGE ACROSS
 !  EITHER OF THE FACES PERPEDICULAR TO THE GIVEN COORDINATE DIRECTION.
 !  IT SETS AND RETURNS A FLAG "IVFLG" TO INDICATE WHAT CONDITION EXISTS
 !  FOR THE GIVEN COORDINATE DIRECTION.
 !
 !     IVFLG = 0 -- "NORMAL" CONDITIONS. OUTFLOW CAN OCCUR ACROSS ONE
 !                   OR BOTH OF THE FACES PERPENDICULAR TO THIS
 !                   DIRECTION.
 !     IVFLG = 1 -- A CONSTANT, NON-ZERO VELOCITY COMPONENT EXISTS IN
 !                  THIS DIRCETION THROUGHOUT THE CELL. DT IS COMPUTED
 !                  USING THE CONSTANT VELOCITY INSTEAD OF THE LOG
 !                  EXPRESSION.
 !     IVFLG = 2 -- "MACHINE" ZERO (V<10**-15) VELOCITY COMPONENT EXISTS
 !                  THROUGHOUT THE CELL IN THIS DIRECTION.
 !                  DT IS SET TO "MACHINE" INFINITY (10**20).
 !     IVFLG = 3 -- THERE IS NO POTENTIAL FOR OUTFLOW FROM EITHER FACE
 !                  PERPENDICULAR TO THIS DIRECTION.
 !                  DT IS SET TO "MACHINE" INFINITY (10**20).
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: IVFLG
 REAL,INTENT(IN)     :: V1,V2,X1,X2,XP
 REAL,INTENT(OUT)    :: DT,V,DVDX

 REAL                :: V2A,V1A,DV,DVA,VV,VVV,ZRO,ZROM,DX,XF, &
                        VR1,VR2,VR,V1V2

 IVFLG=0
 
 DT = PLUSINFINITY 
 V2A= ABS(V2)
 V1A= ABS(V1)
 DV = V2-V1
 DVA= ABS(DV)

 !#####check for a uniform zero velocity in this direction
 !#####if so, return with dt set to 1e+20 and ivflg=2
 IF(V2A.LE.ACCURACY.AND.V1A.LE.ACCURACY)THEN
  IVFLG=2
  RETURN
 ENDIF

 !#####check for a constant uniform non-zero velocity in this direction.
 !#####variable vvv can only be << 0 if v1 and v2 are in the same
 !#####direction. if vvv < 1e-4, v1 and v2 are essentially equal. if
 !#####this condition exists, set v=v1 and compute travel
 !#####time using constant velocity. set ivflg=1.
 VV=V1A
 IF(V2A.GT.VV)VV=V2A
 VVV=DVA/VV
 IF(VVV.LE.1.0E-4)THEN
  IVFLG= 1
  ZRO  = ACCURACY
  ZROM =-ZRO
  V=V1
  IF(V1.GT.ZRO) DT=(X2-XP)/V1
  IF(V1.LT.ZROM)DT=(X1-XP)/V1
  RETURN
 ENDIF

 !#####compute velocity corresponding to particle's postion
 DX  =X2-X1
 DVDX=DV/DX
 XF  =(XP-X1)/DX
 !## due to numerical inaccuracy --- COMPILER
 XF=MAX(0.0,MIN(XF,1.0))
 V   =(1.0E+0-XF)*V1+XF*V2

 !#####if flow is into cell from both sides there is no outflow, so
 !#####set ivflg=3 and return
 IF(V1.GE.0.0E+0.AND.V2.LE.0.0E+0)THEN
  IVFLG=3
  RETURN
 ENDIF

 !#####if flow is out of cell on both sides, find location of divide,
 !#####compute travel time to exit face and return with ivflg=0
 IF(V1.LE.0.0.AND.V2.GE.0.0)THEN
 !######zero velocity on divide
  IF(ABS(V).LE.0.0)THEN
   V=MINUSINFINITY
   IF(V2.LE.0.0)V=-V
  END IF
 END IF
 VR1=V1/V
 VR2=V2/V
 VR =VR1
 IF(VR.LE.0.0E+0)VR=VR2
 V1V2=V1*V2
 IF(V1V2.GT.0.0)THEN
  IF(V.GT.0.0)VR=VR2
  IF(V.LT.0.0)VR=VR1
 ENDIF
 DT=ALOG(VR)/DVDX

 END SUBROUTINE DTCALC

END MODULE MOD_PLINES_FLOLIN

