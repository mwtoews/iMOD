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
MODULE MOD_STOMP

USE MOD_STOMP_PAR
USE MOD_IDF, ONLY : IDFDEALLOCATE,IDFCOPY,IDFREADSCALE,IDFREAD,IDFNULLIFY,UTL_WRITE_FREE_ROW,IDFFILLSXSY,IDFWRITE, &
  IDFALLOCATEX,IDFREADCROSS
USE MOD_UTL, ONLY : UTL_IDFSNAPTOGRID_LLC,UTL_READINITFILE,ITOS,UTL_GETUNIT,UTL_CREATEDIR,RTOS,ITOS
USE MOD_OSD, ONLY : OSD_GETENV,OSD_DATE_AND_TIME
CHARACTER(LEN=256),PRIVATE :: LINE

INTEGER,PRIVATE :: ILAY1,ILAY2,DLAY

CONTAINS

 !###======================================================================
 LOGICAL FUNCTION STOMP_SAVEINPUT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IU

 STOMP_SAVEINPUT=.FALSE.

 IU=UTL_GETUNIT(); OPEN(IU,FILE=TRIM(OUTPUTFILE)//'\input',STATUS='UNKNOWN')

 !## reverse order for ijk indexing
 ILAY1=NLAY
 ILAY2=1
 DLAY =-1

 CALL STOMP_WRITE_SIM_TITLE(IU)
 CALL STOMP_WRITE_SOL_CNTRL(IU)
 CALL STOMP_WRITE_GRID(IU)
 CALL STOMP_WRITE_SOILZONATION(IU)
 CALL STOMP_WRITE_INACTIVE(IU)
 CALL STOMP_WRITE_MECHANICAL(IU)
 CALL STOMP_WRITE_HYDRAULICPROPERTIES(IU)
 CALL STOMP_WRITE_SAT_FUNC(IU)
 CALL STOMP_WRITE_AQ_RE_PERM(IU)
 CALL STOMP_WRITE_GAS_REL_PERM(IU)
 CALL STOMP_WRITE_INITIAL_CONDITIONS(IU)
 CALL STOMP_WRITE_THERMAL_PROPERTY(IU)
 CALL STOMP_WRITE_OUTPUT(IU)
 !## save stomp.idf
 IF(.NOT.IDFWRITE(BND(1),TRIM(OUTPUTFILE)//'\stomp.idf',1))THEN; ENDIF
 
 CLOSE(IU)

 STOMP_SAVEINPUT=.TRUE.

 END FUNCTION STOMP_SAVEINPUT

 !###======================================================================
 SUBROUTINE STOMP_WRITE_SIM_TITLE(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 CHARACTER(LEN=52) :: DATESTRING

 WRITE(IU,'(A)') ''
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') '~Simulation Title Card'
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') '1,'                              !## version number
 WRITE(IU,'(A)') 'MediCinHat Simulation,'          !## simulation title
 WRITE(IU,'(A)') TRIM(OSD_GETENV('USERNAME'))//',' !## username
 WRITE(IU,'(A)') 'Deltares,'                       !## company name
 CALL OSD_DATE_AND_TIME(DATEANDTIME=DATESTRING)
 WRITE(IU,'(A)') DATESTRING(:10)//','              !## input creation date
 WRITE(IU,'(A)') DATESTRING(13:19)//','            !## input creation time
 WRITE(IU,'(A)') '0,'                              !## number of simulation note lines

 END SUBROUTINE STOMP_WRITE_SIM_TITLE

 !###======================================================================
 SUBROUTINE STOMP_WRITE_SOL_CNTRL(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU

 WRITE(IU,'(A)') ''
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') '~Solution Control Card'
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') 'Normal,'
 WRITE(IU,'(A)') 'STOMP-GT,'
 WRITE(IU,'(A)') '1,'
 WRITE(IU,'(A)') '0.0,day,20.0,yr,0.01,sec,50,day,1.25,16,1.e-06,'
 WRITE(IU,'(A)') '10000,'   !## maximum number of timesteps
 WRITE(IU,'(A)') 'Variable Aqueous Diffusion,'
 WRITE(IU,'(A)') 'Variable Gas Diffusion,'
 WRITE(IU,'(A)') '0,'

 END SUBROUTINE STOMP_WRITE_SOL_CNTRL

 !###======================================================================
 SUBROUTINE STOMP_WRITE_SAT_FUNC(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 REAL :: A,P,K,N
 INTEGER :: JU,ILAY,IROW,ICOL

 WRITE(IU,'(A)') ''
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') '~Saturation Function Card'
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') 'IJK Indexing,van Genuchten with Entrapment,file:alpha.dat,1/m,file:n.dat,0.346,,0.15,2.0e-5,'

 JU=UTL_GETUNIT(); OPEN(JU,FILE=TRIM(OUTPUTFILE)//'\alpha.dat',STATUS='UNKNOWN')
 DO ILAY=ILAY1,ILAY2,DLAY; DO IROW=1,NROW; DO ICOL=1,NCOL
  K=KHV(ILAY,1)%X(ICOL,IROW)
  CALL STOMP_GET_A_AND_N(K,A,N)
  WRITE(JU,'(G15.7)') A
 ENDDO; ENDDO; ENDDO
 CLOSE(JU)

 JU=UTL_GETUNIT(); OPEN(JU,FILE=TRIM(OUTPUTFILE)//'\n.dat',STATUS='UNKNOWN')
 DO ILAY=ILAY1,ILAY2,DLAY; DO IROW=1,NROW; DO ICOL=1,NCOL
  K=KHV(ILAY,1)%X(ICOL,IROW)
  CALL STOMP_GET_A_AND_N(K,A,N)
  N=MAX(1.00001,N)
  WRITE(JU,'(G15.7)') N
 ENDDO; ENDDO; ENDDO
 CLOSE(JU)

 END SUBROUTINE STOMP_WRITE_SAT_FUNC

 !###======================================================================
 SUBROUTINE STOMP_GET_A_AND_N(K,A,N)
 !###======================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: K
 REAL,INTENT(OUT) :: A,N
 REAL :: X,M,KD,KM
 
 !## k in milidarcy
 KD=K*0.831*1000
 !## 1/kPa
 A=2.01*(7.37*KD**-0.43)**-1.20
 A=A*0.10199773339984

 KM=KD*9.869233E-13
 X=(LOG10(KM)+20.34)/3.94
 M=0.5-0.5*ERF(X)

 N=-1.0/(M-1.0)
 N=MAX(1.25,N)
 
 END SUBROUTINE STOMP_GET_A_AND_N
 
 !###======================================================================
 SUBROUTINE STOMP_WRITE_AQ_RE_PERM(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU

 WRITE(IU,'(A)') ''
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') '~Aqueous Relative Permeability Card'
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') 'IJK Indexing,Mualem,,'

 END SUBROUTINE STOMP_WRITE_AQ_RE_PERM

 !###======================================================================
 SUBROUTINE STOMP_WRITE_GAS_REL_PERM(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU

 WRITE(IU,'(A)') ''
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') '~Gas Relative Permeability Card'
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') 'IJK Indexing,Mualem,,'

 END SUBROUTINE STOMP_WRITE_GAS_REL_PERM

 !###======================================================================
 SUBROUTINE STOMP_WRITE_INACTIVE(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: JU,IROW,ICOL,ILAY

 WRITE(IU,'(A)') ''
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') '~Inactive Nodes Card'
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') 'file,inactive.dat,'

 JU=UTL_GETUNIT(); OPEN(JU,FILE=TRIM(OUTPUTFILE)//'\inactive.dat',STATUS='REPLACE')
 DO ILAY=ILAY1,ILAY2,DLAY; DO IROW=1,NROW; DO ICOL=1,NCOL
  IF(BND(ILAY)%X(ICOL,IROW).EQ.0)WRITE(JU,'(3I10)') ICOL,IROW,ILAY
 ENDDO; ENDDO; ENDDO
 CLOSE(JU)

 END SUBROUTINE STOMP_WRITE_INACTIVE

 !###======================================================================
 SUBROUTINE STOMP_WRITE_SOILZONATION(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: JU,IROW,ICOL,ILAY

 WRITE(IU,'(A)') ''
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') '~Rock/Soil Zonation Card'
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') 'IJK Indexing,'

 END SUBROUTINE STOMP_WRITE_SOILZONATION

 !###======================================================================
 SUBROUTINE STOMP_WRITE_MECHANICAL(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: JU,IROW,ICOL,ILAY
 REAL :: K,P
 
 WRITE(IU,'(A)') ''
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') '~Mechanical Properties Card'
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') 'IJK Indexing,2690,kg/m^3,file:por.dat,file:por.dat,,1/m,Millington and Quirk,'

 JU=UTL_GETUNIT(); OPEN(JU,FILE=TRIM(OUTPUTFILE)//'\por.dat',STATUS='UNKNOWN')
 DO ILAY=ILAY1,ILAY2,DLAY; DO IROW=1,NROW; DO ICOL=1,NCOL
  K=KHV(ILAY,1)%X(ICOL,IROW)
  P=STOMP_GETPOROSITY(K)
  WRITE(JU,'(G15.7)') P
 ENDDO; ENDDO; ENDDO
 CLOSE(JU)

 END SUBROUTINE STOMP_WRITE_MECHANICAL

 !###======================================================================
 SUBROUTINE STOMP_WRITE_HYDRAULICPROPERTIES(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: JU,IROW,ICOL,ILAY
 REAL :: K1,K2,K3,K4,D

 WRITE(IU,'(A)') ''
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') '~Hydraulic Properties Card'
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') 'IJK Indexing, file:ksx.dat,Darcy, file:ksy.dat,Darcy, file:ksz.dat,Darcy,'

 JU=UTL_GETUNIT(); OPEN(JU,FILE=TRIM(OUTPUTFILE)//'\ksx.dat',STATUS='UNKNOWN')
 DO ILAY=ILAY1,ILAY2,DLAY; DO IROW=1,NROW; DO ICOL=1,NCOL
  D=1.0/0.831
  WRITE(JU,'(G15.7)') KHV(ILAY,1)%X(ICOL,IROW)*D
 ENDDO; ENDDO; ENDDO
 CLOSE(JU)

 JU=UTL_GETUNIT(); OPEN(JU,FILE=TRIM(OUTPUTFILE)//'\ksy.dat',STATUS='UNKNOWN')
 DO ILAY=ILAY1,ILAY2,DLAY; DO IROW=1,NROW; DO ICOL=1,NCOL
  D=1.0/0.831
  WRITE(JU,'(G15.7)') KHV(ILAY,2)%X(ICOL,IROW)*D
 ENDDO; ENDDO; ENDDO
 CLOSE(JU)

 JU=UTL_GETUNIT(); OPEN(JU,FILE=TRIM(OUTPUTFILE)//'\ksz.dat',STATUS='UNKNOWN')
 DO ILAY=ILAY1,ILAY2,DLAY; DO IROW=1,NROW; DO ICOL=1,NCOL
  D=1.0/0.831
  WRITE(JU,'(G15.7)') KHV(ILAY,3)%X(ICOL,IROW)*D
 ENDDO; ENDDO; ENDDO
 CLOSE(JU)

 END SUBROUTINE STOMP_WRITE_HYDRAULICPROPERTIES

 !###======================================================================
 SUBROUTINE STOMP_WRITE_INITIAL_CONDITIONS(IU)
 !###======================================================================
 IMPLICIT NONE
 REAL,PARAMETER :: RHO=1000.0
 REAL,PARAMETER :: G=9.8
 REAL,PARAMETER :: PAIR=101325.0
 INTEGER,INTENT(IN) :: IU
 INTEGER :: JU,IROW,ICOL,ILAY,JLAY
 REAL :: HB,WP,T,GP
 LOGICAL :: LWCONSTANT,LGCONSTANT
 
 LWCONSTANT=.TRUE.
 LGCONSTANT=.FALSE.
 
 WRITE(IU,'(A)') ''
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') '~Initial Conditions Card'
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') 'Aqueous Saturation,Gas Pressure,'
 WRITE(IU,'(A)') '2,'
 WRITE(IU,'(A)') 'Aqueous Saturation File,,,ini_aqueous.dat,'
 WRITE(IU,'(A)') 'Gas Pressure File,,Pa,ini_gas.dat,'

 !## relation with depth
 JU=UTL_GETUNIT(); OPEN(JU,FILE=TRIM(OUTPUTFILE)//'\ini_aqueous.dat',STATUS='UNKNOWN')
 JLAY=0
 DO ILAY=ILAY1,ILAY2,DLAY; JLAY=JLAY+1; DO IROW=1,NROW; DO ICOL=1,NCOL
  IF(LWCONSTANT)THEN
!   WP=1.0-WAT(ILAY)%X(ICOL,IROW)
   WP=MAX(0.2,WAT(ILAY)%X(ICOL,IROW))
!   IF(JLAY.GT.NLAY-3)THEN
!    WP=1.0
!   ELSE
!    WP=0.5
!   ENDIF
  ELSE
   !## initial pressure assigned to centroid of cell
   T=TB(ILAY,1)%X(ICOL,IROW)-TB(ILAY,2)%X(ICOL,IROW)  
   IF(T.LE.0.0)THEN
    WRITE(*,'(/1X,A,F10.2,A,3I5)') 'Error, thickness (',T,') less than zero',ICOL,IROW,JLAY
    STOP
   ENDIF
   !## get total depth up to mid of current modellayer
   T=(SLEVEL%X(ICOL,IROW)-TB(ILAY,1)%X(ICOL,IROW))+0.5*T
   HB=T
   WP=RHO*G*HB+PAIR
  ENDIF
  WRITE(JU,'(3I10,G15.7)') ICOL,IROW,JLAY,WP
 ENDDO; ENDDO; ENDDO
 CLOSE(JU)
  
 JU=UTL_GETUNIT(); OPEN(JU,FILE=TRIM(OUTPUTFILE)//'\ini_gas.dat',STATUS='UNKNOWN')
 JLAY=0
 DO ILAY=ILAY1,ILAY2,DLAY; JLAY=JLAY+1; DO IROW=1,NROW; DO ICOL=1,NCOL
  IF(LGCONSTANT)THEN
   GP=101325.0+1.0E-9
  ELSE
   !## deep pressure (kPA)
   GP=4580000.0 !  3035795
   !## initial pressure assigned to centroid of cell
   T=TB(ILAY,1)%X(ICOL,IROW)-TB(ILAY,2)%X(ICOL,IROW)  
   IF(T.LE.0.0D0)THEN
    WRITE(*,'(/1X,A,F10.2,A,3I5)') 'Error, thickness (',T,') less than zero',ICOL,IROW,JLAY; STOP
   ENDIF
   !## get total depth up to mid of current modellayer
   T =(TB(ILAY,2)%X(ICOL,IROW)-TB(ILAY1,2)%X(ICOL,IROW))+0.5*T
   HB=T
   GP=GP-(205*HB)
  ENDIF
  WRITE(JU,'(3I10,G15.7)') ICOL,IROW,JLAY,GP
 ENDDO; ENDDO; ENDDO
 CLOSE(JU)

 END SUBROUTINE STOMP_WRITE_INITIAL_CONDITIONS

 !###======================================================================
 SUBROUTINE STOMP_WRITE_THERMAL_PROPERTY(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: JU,IROW,ICOL,ILAY

 WRITE(IU,'(A)') ''
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') '~Thermal Properties Card'
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') 'IJK Indexing, Somerton,file:gtc.dat,W/m K,file:gtc.dat,W/m K,file:gtc.dat,W/m K,'// &
                 'file:gtc.dat,W/m K,file:gtc.dat,W/m K,file:gtc.dat,W/m K,file:gsh.dat,J/kg K,'

 JU=UTL_GETUNIT(); OPEN(JU,FILE=TRIM(OUTPUTFILE)//'\gtc.dat',STATUS='UNKNOWN')
 DO ILAY=ILAY1,ILAY2,DLAY; DO IROW=1,NROW; DO ICOL=1,NCOL
  WRITE(JU,'(G15.7)') 5.0
 ENDDO; ENDDO; ENDDO
 CLOSE(JU)
 JU=UTL_GETUNIT(); OPEN(JU,FILE=TRIM(OUTPUTFILE)//'\gsh.dat',STATUS='UNKNOWN')
 DO ILAY=ILAY1,ILAY2,DLAY; DO IROW=1,NROW; DO ICOL=1,NCOL
  WRITE(JU,'(G15.7)') 775.0
 ENDDO; ENDDO; ENDDO
 CLOSE(JU)

 END SUBROUTINE STOMP_WRITE_THERMAL_PROPERTY

 !###======================================================================
 SUBROUTINE STOMP_WRITE_OUTPUT(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU

 WRITE(IU,'(A)') ''
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') '~Output Control Card'
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') '0,'  !## number of reference nodes (echo on screen)
 WRITE(IU,'(A)') '1,1,hr,cm,6,6,6,' !## output screen intervals
 WRITE(IU,'(A)') '0,'  !## number of reference variables (echo on screen)
 WRITE(IU,'(A)') '2,'         !## number of plot files
 WRITE(IU,'(A)') '1,year,'   !## number of plot files
 WRITE(IU,'(A)') '10,year,'  !## number of plot files
 WRITE(IU,'(A)') '6,'         !## number of output variables
 WRITE(IU,'(A)') 'aqueous saturation,,'        
 WRITE(IU,'(A)') 'aqueous pressure,,' !## aqueous pressure
 WRITE(IU,'(A)') 'gas saturation,,'            
 WRITE(IU,'(A)') 'gas pressure,Pa,'
 WRITE(IU,'(A)') 'aqueous relative perm,,' !## aqueous relative permeability
 WRITE(IU,'(A)') 'trapped gas sat,,'  !## trapped gas saturation,'

 END SUBROUTINE STOMP_WRITE_OUTPUT
 
 !###======================================================================
 SUBROUTINE STOMP_WRITE_GRID(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: JU,KU,I,J,K,IROW,ICOL,ILAY,E
 REAL,DIMENSION(2) :: X,Y,Z
 INTEGER,DIMENSION(8) :: IX,IY,IZ
 REAL :: XC,YC,ZC
 DATA IX/1,2,1,2,1,2,1,2/
 DATA IY/2,2,1,1,2,2,1,1/
 DATA IZ/1,1,1,1,2,2,2,2/

 WRITE(IU,'(A)') ''
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') '~Grid Card'
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') 'Element and Vertices,'

 !## ncol,nrow,nlay
 LINE=TRIM(ITOS(NCOL))//','//TRIM(ITOS(NROW))//','//TRIM(ITOS(NLAY))//','; WRITE(IU,'(A)') TRIM(LINE)
 LINE='vertices.dat,'//TRIM(ITOS(8*NCOL*NROW*NLAY))//','; WRITE(IU,'(A)') TRIM(LINE)
 LINE='elements.dat,'; WRITE(IU,'(A)') TRIM(LINE)

 JU=UTL_GETUNIT(); OPEN(JU,FILE=TRIM(OUTPUTFILE)//'\vertices.dat',STATUS='UNKNOWN')
 KU=UTL_GETUNIT(); OPEN(KU,FILE=TRIM(OUTPUTFILE)//'\elements.dat',STATUS='UNKNOWN')

 IF(.NOT.IDFFILLSXSY(BND(1)))RETURN

 WRITE(JU,'(A)') 'x[m],y[m],z[m]'

 !## 1 2 4 3 - bottom
 !## 5 6 8 7 - top
 I=0; E=0; DO ILAY=ILAY1,ILAY2,DLAY; DO IROW=1,NROW; DO ICOL=1,NCOL

  X(1)=BND(1)%SX(ICOL-1)
  X(2)=BND(1)%SX(ICOL  )
  Y(1)=BND(1)%SY(IROW-1)
  Y(2)=BND(1)%SY(IROW  )
  Z(2)=TB(ILAY,1)%X(ICOL,IROW) !## top
  Z(1)=TB(ILAY,2)%X(ICOL,IROW) !## bot

  IF(Z(2).EQ.TB(ILAY,1)%NODATA)Z(2)=-999.99
  IF(Z(1).EQ.TB(ILAY,2)%NODATA)Z(1)=-999.99

  E=E+1
  WRITE(KU,'(9I10)') E,(K,K=I+1,I+8)

  DO J=1,8

   XC=X(IX(J))
   YC=Y(IY(J))
   ZC=Z(IZ(J))

   I=I+1
   WRITE(JU,'(I10,3(A1,F10.2),A1)') I,',',XC,',',YC,',',ZC,','

  ENDDO

 ENDDO; ENDDO; ENDDO

 CLOSE(JU); CLOSE(KU)

 END SUBROUTINE STOMP_WRITE_GRID

 !###======================================================================
 LOGICAL FUNCTION STOMP_READ(IU,IDF,IWINDOW,ICROSS)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 INTEGER,INTENT(IN) :: IWINDOW,IU,ICROSS
 INTEGER :: I,J,K,II,ILAY,IROW,ICOL
 REAL :: K1,K2,K3,K4,T,B,D,DX,DY,DXY,A
 CHARACTER(LEN=256) :: LINE

 STOMP_READ=.FALSE.

 NLAY=SIZE(BND)

 IF(IWINDOW.EQ.1)CALL UTL_IDFSNAPTOGRID_LLC(IDF%XMIN,IDF%XMAX,IDF%YMIN,IDF%YMAX,IDF%DX,IDF%DY,IDF%NCOL,IDF%NROW,.TRUE.)
 IF(ICROSS.EQ.1)THEN
  DX=(IDF%XMAX-IDF%XMIN); DY=(IDF%YMAX-IDF%YMIN); DXY=DX**2.0+DY**2.0; IF(DXY.GT.0.0)DXY=SQRT(DXY)
  IDF%NROW=1; IDF%NCOL=DXY/IDF%DX; A=ATAN(DY/DX); DXY=IDF%NCOL*IDF%DX
  !## get upper corner
  IDF%XMAX=IDF%XMIN+COS(A)*DXY; IDF%YMAX=IDF%YMIN+SIN(A)*DXY
 ENDIF
 
 IF(.NOT.UTL_READINITFILE('SURFLEVEL',LINE,IU,0))RETURN
 READ(LINE,*) SLEVEL%FNAME; LINE='SURFLEVEL='//TRIM(SLEVEL%FNAME); WRITE(*,'(A)') TRIM(LINE)

 DO I=1,NLAY
  IF(.NOT.UTL_READINITFILE('BND_L'//TRIM(ITOS(I)),LINE,IU,0))RETURN
  READ(LINE,*) BND(I)%FNAME; LINE='BND_L'//TRIM(ITOS(I))//'='//TRIM(BND(I)%FNAME); WRITE(*,'(A)') TRIM(LINE)
 ENDDO
 DO I=1,NLAY
  DO J=1,SIZE(KHV,2)
   IF(.NOT.UTL_READINITFILE(TRIM(TXT(J))//'_L'//TRIM(ITOS(I)),LINE,IU,0))RETURN
   READ(LINE,*) KHV(I,J)%FNAME; LINE=TRIM(TXT(J))//'_L'//TRIM(ITOS(I))//'='//TRIM(KHV(I,J)%FNAME); WRITE(*,'(A)') TRIM(LINE)
  ENDDO
  IF(.NOT.UTL_READINITFILE('WAT_L'//TRIM(ITOS(I)),LINE,IU,0))RETURN
  READ(LINE,*) WAT(I)%FNAME; LINE='WAT_L'//TRIM(ITOS(I))//'='//TRIM(WAT(I)%FNAME); WRITE(*,'(A)') TRIM(LINE)
 ENDDO
 DO I=1,NLAY
  IF(.NOT.UTL_READINITFILE('TOP_L'//TRIM(ITOS(I)),LINE,IU,0))RETURN
  READ(LINE,*) TB(I,1)%FNAME; LINE='TOP_L'//TRIM(ITOS(I))//'='//TRIM(TB(I,1)%FNAME); WRITE(*,'(A)') TRIM(LINE)
  IF(.NOT.UTL_READINITFILE('BOT_L'//TRIM(ITOS(I)),LINE,IU,0))RETURN
  READ(LINE,*) TB(I,2)%FNAME; LINE='BOT_L'//TRIM(ITOS(I))//'='//TRIM(TB(I,2)%FNAME); WRITE(*,'(A)') TRIM(LINE)
 ENDDO
 IF(.NOT.UTL_READINITFILE('OUTPUTMAP',LINE,IU,0))RETURN
 READ(LINE,*) OUTPUTFILE; WRITE(*,'(A)') 'OUTPUTMAP='//TRIM(OUTPUTFILE)
 CALL UTL_CREATEDIR(OUTPUTFILE)
 CALL UTL_CREATEDIR(TRIM(OUTPUTFILE)//'\INPUT_IDF')

 IF(ICROSS.EQ.1)THEN

  IF(.NOT.IDFALLOCATEX(IDF))RETURN

  CALL IDFCOPY(IDF,SLEVEL)
  IF(.NOT.IDFREADCROSS(SLEVEL,SLEVEL%FNAME))RETURN
  SLEVEL%FNAME=TRIM(OUTPUTFILE)//'\INPUT_IDF\'//TRIM(SLEVEL%FNAME(INDEX(SLEVEL%FNAME,'\',.TRUE.):))
  IF(.NOT.IDFWRITE(SLEVEL,SLEVEL%FNAME,1))RETURN

  K=0
  DO I=1,NLAY
   CALL IDFCOPY(IDF,BND(I))
   IF(.NOT.IDFREADCROSS(BND(I),BND(I)%FNAME))RETURN
   BND(I)%FNAME=TRIM(OUTPUTFILE)//'\INPUT_IDF\'//TRIM(BND(I)%FNAME(INDEX(BND(I)%FNAME,'\',.TRUE.):))
   IF(.NOT.IDFWRITE(BND(I),BND(I)%FNAME,1))RETURN
   K=K+1
   DO J=1,SIZE(TB,2)
    CALL IDFCOPY(IDF,TB(I,J))   
    IF(.NOT.IDFREADCROSS(TB(I,J),TB(I,J)%FNAME))RETURN
    IF(J.EQ.1)THEN
     TB(I,J)%FNAME=TRIM(OUTPUTFILE)//'\INPUT_IDF\'//TRIM(ITOS(K))//'_'//TRIM(TB(I,J)%FNAME(INDEX(TB(I,J)%FNAME,'\',.TRUE.)+1:))
    ELSE
     TB(I,J)%FNAME=TRIM(OUTPUTFILE)//'\INPUT_IDF\'//TRIM(TB(I,J)%FNAME(INDEX(TB(I,J)%FNAME,'\',.TRUE.):))
    ENDIF
    IF(.NOT.IDFWRITE(TB(I,J),TB(I,J)%FNAME,1))RETURN
   ENDDO
   K=K+1
   DO J=1,SIZE(KHV,2)
    CALL IDFCOPY(IDF,KHV(I,J))   
    IF(.NOT.IDFREADCROSS(KHV(I,J),KHV(I,J)%FNAME))RETURN
    IF(J.EQ.1)THEN
     KHV(I,J)%FNAME=TRIM(OUTPUTFILE)//'\INPUT_IDF\'//TRIM(ITOS(K))//'_'//TRIM(KHV(I,J)%FNAME(INDEX(KHV(I,J)%FNAME,'\',.TRUE.)+1:))
    ELSE
     KHV(I,J)%FNAME=TRIM(OUTPUTFILE)//'\INPUT_IDF\'//TRIM(KHV(I,J)%FNAME(INDEX(KHV(I,J)%FNAME,'\',.TRUE.):))
    ENDIF
    IF(.NOT.IDFWRITE(KHV(I,J),KHV(I,J)%FNAME,1))RETURN
   ENDDO

   CALL IDFCOPY(IDF,WAT(I))
   IF(.NOT.IDFREADCROSS(WAT(I),WAT(I)%FNAME))RETURN
   WAT(I)%FNAME=TRIM(OUTPUTFILE)//'\INPUT_IDF\'//TRIM(WAT(I)%FNAME(INDEX(WAT(I)%FNAME,'\',.TRUE.):))
   IF(.NOT.IDFWRITE(WAT(I),WAT(I)%FNAME,1))RETURN
   
  ENDDO
 
 ELSE

  IF(.NOT.IDFREADCROSS(IDF,IDF%FNAME))RETURN
  IF(I.EQ.1.AND.IWINDOW.EQ.0)THEN
   IF(.NOT.IDFREAD(IDF,IDF%FNAME,1))RETURN
  ELSE
   IF(.NOT.IDFREADSCALE(IDF%FNAME,IDF,1,1,0.0,0))RETURN
  ENDIF
!  IDF%FNAME=TRIM(OUTPUTFILE)//'\INPUT_IDF\'//TRIM(IDF%FNAME(INDEX(IDF%FNAME,'\',.TRUE.):))
!  IF(.NOT.IDFWRITE(IDF,IDF%FNAME,1))RETURN

  DO I=1,NLAY
   WRITE(*,'(A,I10)') 'Reading files for layer ',I
   IF(I.EQ.1.AND.IWINDOW.EQ.0)THEN
    IF(.NOT.IDFREAD(BND(I),BND(I)%FNAME,1))RETURN
    CALL IDFCOPY(BND(I),IDF)
   ELSE
    CALL IDFCOPY(IDF,BND(I)); IF(.NOT.IDFREADSCALE(BND(I)%FNAME,BND(I),1,1,0.0,0))RETURN
   ENDIF
   !## reading top/bot as average values
   DO J=1,SIZE(TB,2)
    CALL IDFCOPY(BND(I),TB(I,J))
    IF(.NOT.IDFREADSCALE(TB(I,J)%FNAME,TB(I,J),2,1,0.0,0))RETURN
   ENDDO
   !## reading permeability as geometric means
   DO J=1,SIZE(KHV,2)
    CALL IDFCOPY(BND(I),KHV(I,J))
    IF(.NOT.IDFREADSCALE(KHV(I,J)%FNAME,KHV(I,J)  ,3,1,0.0,0))RETURN
   ENDDO
  ENDDO
 
 ENDIF
 
 NROW=IDF%NROW; NCOL=IDF%NCOL

 !## convert vertical anisotropy into k-vertical
 DO ILAY=1,NLAY; DO IROW=1,NROW; DO ICOL=1,NCOL
  K1=LOG(KHV(ILAY,1)%X(ICOL,IROW))
  K2=LOG(KHV(ILAY,2)%X(ICOL,IROW))
  !## log average permeability
  K3=EXP((K1+K2)/2.0)
  !## vertical permeability
  K4=K3*KHV(ILAY,3)%X(ICOL,IROW)
  !## overwrite vertical anisotropy with vertical permeability
  KHV(ILAY,3)%X(ICOL,IROW)=K4
 ENDDO; ENDDO; ENDDO
 
 !## check zero thicknesses
 DO ILAY=1,NLAY; DO IROW=1,NROW; DO ICOL=1,NCOL
  T=TB(ILAY,1)%X(ICOL,IROW)
  B=TB(ILAY,2)%X(ICOL,IROW)
  D=T-B
  IF(D.LE.0.0)THEN
   WRITE(*,'(A,3I5,A)') 'Zero thickness modellayer ',ICOL,IROW,ILAY,' not possible'; STOP
  ENDIF
 ENDDO; ENDDO; ENDDO
 
 !## check whether isolated cells appear (no connection with conductance)
 II=0; DO ILAY=1,NLAY; DO IROW=1,NROW; DO ICOL=1,NCOL

  !## skip nodata location
  IF(BND(ILAY)%X(ICOL,IROW).EQ.0)CYCLE

  II=II+1
!  IF(II.EQ.135874)THEN
!   WRITE(*,*) KHV(K-1,3)%X(I,J)
!   WRITE(*,*) KHV(K  ,3)%X(I,J)
!   WRITE(*,*) KHV(K  ,2)%X(I,J-1)
!   WRITE(*,*) KHV(K  ,2)%X(I,J)
!   WRITE(*,*) KHV(K  ,1)%X(I-1,J)
!   WRITE(*,*) KHV(K  ,1)%X(I,J)
!  ENDIF

  T=0.0
  IF(ILAY.GT.1   )T=T+KHV(ILAY-1,3)%X(ICOL  ,IROW  )
  IF(ILAY.LT.NLAY)T=T+KHV(ILAY  ,3)%X(ICOL  ,IROW  )
  IF(IROW.GT.1   )T=T+KHV(ILAY  ,2)%X(ICOL  ,IROW-1)    
  IF(IROW.LT.NROW)T=T+KHV(ILAY  ,2)%X(ICOL  ,IROW  )    
  IF(ICOL.GT.1   )T=T+KHV(ILAY  ,1)%X(ICOL-1,IROW  )    
  IF(ICOL.LT.NCOL)T=T+KHV(ILAY  ,1)%X(ICOL  ,IROW  )    
  IF(T.EQ.0.0)THEN
   WRITE(*,'(A,3I5,A)') 'Current cell ',ICOL,IROW,ILAY,' not connected to any other'; STOP
  ENDIF
  
 ENDDO; ENDDO; ENDDO
 
 STOMP_READ=.TRUE.

 END FUNCTION STOMP_READ

 !###======================================================================
 SUBROUTINE STOMP_CLOSE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 IF(ALLOCATED(BND))THEN
  CALL IDFDEALLOCATE(BND,SIZE(BND)); DEALLOCATE(BND)
 ENDIF
 IF(ALLOCATED(TB))THEN
  DO I=1,SIZE(TB,2); CALL IDFDEALLOCATE(TB(:,I),SIZE(TB,1)); ENDDO; DEALLOCATE(TB)
 ENDIF
 IF(ALLOCATED(KHV))THEN
  DO I=1,SIZE(KHV,2); CALL IDFDEALLOCATE(KHV(:,I),SIZE(KHV,1)); ENDDO; DEALLOCATE(KHV)
 ENDIF

 END SUBROUTINE STOMP_CLOSE

 !###======================================================================
 SUBROUTINE STOMP_INIT(NLAY)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NLAY
 INTEGER :: I,J

 ALLOCATE(TB(NLAY,2),KHV(NLAY,3),BND(NLAY),WAT(NLAY))
 DO I=1,NLAY                    ; CALL IDFNULLIFY(BND(I)) ; ENDDO
 DO I=1,NLAY                    ; CALL IDFNULLIFY(WAT(I)) ; ENDDO
 DO I=1,NLAY; DO J=1,SIZE(TB,2) ; CALL IDFNULLIFY(TB(I,J)); ENDDO; ENDDO
 DO I=1,NLAY; DO J=1,SIZE(KHV,2); CALL IDFNULLIFY(KHV(I,J)) ; ENDDO; ENDDO
 CALL IDFNULLIFY(SLEVEL)
 
 END SUBROUTINE STOMP_INIT

 !###======================================================================
 REAL FUNCTION STOMP_GETPOROSITY(K)
 !###======================================================================
 IMPLICIT NONE
 REAL,PARAMETER :: C1=0.018
 REAL,PARAMETER :: C2=0.2958
 REAL,INTENT(INOUT) :: K
 REAL :: P
 
 IF(K.EQ.0.0)K=TINY(1.0)
 P=C1*LOG(K)+C2
  
 STOMP_GETPOROSITY=P
 
 END FUNCTION STOMP_GETPOROSITY
 
END MODULE MOD_STOMP
