!!  Copyright (C) Stichting Deltares, 2005-2018.
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
USE MOD_IDF !, ONLY : IDFDEALLOCATE,IDFCOPY,IDFREADSCALE,IDFREAD,IDFNULLIFY,UTL_WRITE_FREE_ROW,IDFFILLSXSY,IDFWRITE, &
  !IDFALLOCATEX,IDFREADCROSS
USE MOD_UTL, ONLY : UTL_IDFSNAPTOGRID_LLC,UTL_READINITFILE,ITOS,UTL_GETUNIT,UTL_CREATEDIR,RTOS,ITOS
USE MOD_OSD, ONLY : OSD_GETENV,OSD_DATE_AND_TIME
CHARACTER(LEN=256),PRIVATE :: LINE

INTEGER,PRIVATE :: ILAY1,ILAY2,DLAY

CONTAINS

 !###======================================================================
 LOGICAL FUNCTION STOMP_SAVEINPUT(ITYPE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITYPE
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
 CALL STOMP_WRITE_BOUNDARY_CONDITIONS(IU)
 CALL STOMP_WRITE_SOILZONATION(IU)
 CALL STOMP_WRITE_SURFACE_FLUX(IU)
 CALL STOMP_WRITE_INACTIVE(IU)
 CALL STOMP_WRITE_MECHANICAL(IU)
 CALL STOMP_WRITE_HYDRAULICPROPERTIES(IU)
 CALL STOMP_WRITE_SAT_FUNC(IU)
 CALL STOMP_WRITE_AQ_RE_PERM(IU)
 CALL STOMP_WRITE_GAS_REL_PERM(IU)
 CALL STOMP_WRITE_INITIAL_CONDITIONS(IU,ITYPE)
 CALL STOMP_WRITE_THERMAL_PROPERTY(IU)
! CALL STOMP_WRITE_SOURCES(IU)
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
 SUBROUTINE STOMP_WRITE_BOUNDARY_CONDITIONS(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU

 WRITE(IU,'(A)') ''
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') '~Boundary Conditions Card'
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') '1,'
 WRITE(IU,'(A)') 'Top,advective,evaporative,'
 WRITE(IU,'(A)') '1,'//TRIM(ITOS(NCOL))//',1,1,'//TRIM(ITOS(NLAY))//','//TRIM(ITOS(NLAY))//',1,'
 WRITE(IU,'(A)') '0,day,20.11,C,5.0,W/m^2 K,4559807,Pa,0.0,m/s,0.25,'

 END SUBROUTINE STOMP_WRITE_BOUNDARY_CONDITIONS

 !###======================================================================
 SUBROUTINE STOMP_WRITE_SURFACE_FLUX(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU

 WRITE(IU,'(A)') ''
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') '~Surface Flux Card'
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') '8,'
 WRITE(IU,'(A)') 'Aqueous Volumetric Flux,m^3/day,m^3,east,'//TRIM(ITOS(NCOL))//','//TRIM(ITOS(NCOL))// &
       ',1,1,1,'//TRIM(ITOS(NLAY))//','
 WRITE(IU,'(A)') 'Aqueous Volumetric Flux,m^3/day,m^3,north,1,'//TRIM(ITOS(NCOL))//',1,1,'//TRIM(ITOS(NLAY))//','//TRIM(ITOS(NLAY))//','
 WRITE(IU,'(A)') 'Aqueous Volumetric Flux,m^3/day,m^3,west,1,1,1,1,1,'//TRIM(ITOS(NLAY))//','
 WRITE(IU,'(A)') 'Aqueous Volumetric Flux,m^3/day,m^3,south,1,'//TRIM(ITOS(NCOL))//',1,1,1,1,'
 WRITE(IU,'(A)') 'Gas Volumetric Flux,m^3/day,m^3,east,'//TRIM(ITOS(NCOL))//','//TRIM(ITOS(NCOL))//',1,1,1,'//TRIM(ITOS(NLAY))//','
 WRITE(IU,'(A)') 'Gas Volumetric Flux,m^3/day,m^3,north,1,'//TRIM(ITOS(NCOL))//',1,1,'//TRIM(ITOS(NLAY))//','//TRIM(ITOS(NLAY))//','
 WRITE(IU,'(A)') 'Gas Volumetric Flux,m^3/day,m^3,west,1,1,1,1,1,'//TRIM(ITOS(NLAY))//','
 WRITE(IU,'(A)') 'Gas Volumetric Flux,m^3/day,m^3,south,1,'//TRIM(ITOS(NCOL))//',1,1,1,1,'

 END SUBROUTINE STOMP_WRITE_SURFACE_FLUX

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
! WRITE(IU,'(A)') '0.0,day,20.0,yr,0.01,sec,50,day,1.25,16,1.e-06,'
 WRITE(IU,'(A)') '0,s,500,year,0.01,s,500,year,1.25,16,1.e-3,'
 WRITE(IU,'(A)') '10000000,'   !## maximum number of timesteps
 WRITE(IU,'(A)') 'No Aqueous Diffusion,'
 WRITE(IU,'(A)') 'No Diffusion,'
! WRITE(IU,'(A)') 'Variable Aqueous Diffusion,'
! WRITE(IU,'(A)') 'Variable Gas Diffusion,'
 WRITE(IU,'(A)') '0,'

 END SUBROUTINE STOMP_WRITE_SOL_CNTRL

 !###======================================================================
 SUBROUTINE STOMP_WRITE_SAT_FUNC(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 REAL(KIND=DP_KIND) :: A,K,N
 INTEGER :: JU,ILAY,IROW,ICOL

 WRITE(IU,'(A)') ''
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') '~Saturation Function Card'
 WRITE(IU,'(A)') '#-------------------------------------------------------'
! WRITE(IU,'(A)') 'IJK Indexing,van Genuchten with Entrapment,file:alpha.dat,1/m,file:n.dat,0.346,,0.15,2.0e-5,'
 WRITE(IU,'(A)') 'IJK Indexing,van Genuchten,file:alpha.dat,1/m,file:n.dat,0.346,,,,'

 JU=UTL_GETUNIT(); OPEN(JU,FILE=TRIM(OUTPUTFILE)//'\alpha.dat',STATUS='UNKNOWN')
 DO ILAY=ILAY1,ILAY2,DLAY; DO IROW=1,NROW; DO ICOL=1,NCOL
  K=KHV(ILAY,1)%X(ICOL,IROW)
  CALL STOMP_GET_A_AND_N(K,A,N)
  WRITE(JU,'(F15.3)') A
 ENDDO; ENDDO; ENDDO
 CLOSE(JU)

 JU=UTL_GETUNIT(); OPEN(JU,FILE=TRIM(OUTPUTFILE)//'\n.dat',STATUS='UNKNOWN')
 DO ILAY=ILAY1,ILAY2,DLAY; DO IROW=1,NROW; DO ICOL=1,NCOL
  K=KHV(ILAY,1)%X(ICOL,IROW)
  CALL STOMP_GET_A_AND_N(K,A,N)
  N=MAX(1.00001,N)
  WRITE(JU,'(F15.3)') N
 ENDDO; ENDDO; ENDDO
 CLOSE(JU)

 END SUBROUTINE STOMP_WRITE_SAT_FUNC

 !###======================================================================
 SUBROUTINE STOMP_GET_A_AND_N(K,A,N)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: K
 REAL(KIND=DP_KIND),INTENT(OUT) :: A,N
 REAL(KIND=DP_KIND) :: X,M,KD,KM
 
 !## k in milidarcy
 KD=K*0.831D0*1000.D0
 !## 1/kPa
 A=2.01D0*(7.37D0*KD**-0.43D0)**-1.20D0
 A=A*0.10199773339984D0

 KM=KD*9.869233D-13
 X=(LOG10(KM)+20.34D0)/3.94D0
 M=0.5D0-0.5D0*ERF(X)

 N=-1.0D0/(M-1.0D0)
 N=MAX(1.25D0,N)
 
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
 REAL(KIND=DP_KIND) :: K,P
 TYPE(IDFOBJ) :: IDF
  
 WRITE(IU,'(A)') ''
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') '~Mechanical Properties Card'
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') 'IJK Indexing,2690,kg/m^3,file:por.dat,file:por.dat,,1/m,Millington and Quirk,'

 CALL IDFCOPY(BND(1),IDF)

 JU=UTL_GETUNIT(); OPEN(JU,FILE=TRIM(OUTPUTFILE)//'\por.dat',STATUS='UNKNOWN')
 DO ILAY=ILAY1,ILAY2,DLAY
  DO IROW=1,NROW; DO ICOL=1,NCOL
   K=KHV(ILAY,1)%X(ICOL,IROW)
   P=STOMP_GETPOROSITY(K)
   WRITE(JU,'(F15.3)') P
   IDF%X(ICOL,IROW)=P
  ENDDO; ENDDO
  IF(.NOT.IDFWRITE(IDF,TRIM(OUTPUTFILE)//'\INPUT_IDF\POR_L'//TRIM(ITOS(ILAY))//'.IDF',1))STOP
 ENDDO
 CLOSE(JU)

 END SUBROUTINE STOMP_WRITE_MECHANICAL

 !###======================================================================
 SUBROUTINE STOMP_WRITE_HYDRAULICPROPERTIES(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: JU,IROW,ICOL,ILAY
 REAL(KIND=DP_KIND) :: D

 WRITE(IU,'(A)') ''
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') '~Hydraulic Properties Card'
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') 'IJK Indexing, file:ksx.dat,Darcy, file:ksy.dat,Darcy, file:ksz.dat,Darcy,'

 JU=UTL_GETUNIT(); OPEN(JU,FILE=TRIM(OUTPUTFILE)//'\ksx.dat',STATUS='UNKNOWN')
 DO ILAY=ILAY1,ILAY2,DLAY; DO IROW=1,NROW; DO ICOL=1,NCOL
  D=1.0D0/0.831D0
  WRITE(JU,'(F15.3)') KHV(ILAY,1)%X(ICOL,IROW)*D
 ENDDO; ENDDO; ENDDO
 CLOSE(JU)

 JU=UTL_GETUNIT(); OPEN(JU,FILE=TRIM(OUTPUTFILE)//'\ksy.dat',STATUS='UNKNOWN')
 DO ILAY=ILAY1,ILAY2,DLAY; DO IROW=1,NROW; DO ICOL=1,NCOL
  D=1.0D0/0.831D0
  WRITE(JU,'(F15.3)') KHV(ILAY,2)%X(ICOL,IROW)*D
 ENDDO; ENDDO; ENDDO
 CLOSE(JU)

 JU=UTL_GETUNIT(); OPEN(JU,FILE=TRIM(OUTPUTFILE)//'\ksz.dat',STATUS='UNKNOWN')
 DO ILAY=ILAY1,ILAY2,DLAY; DO IROW=1,NROW; DO ICOL=1,NCOL
  D=1.0D0/0.831D0
  WRITE(JU,'(F15.3)') KHV(ILAY,3)%X(ICOL,IROW)*D
 ENDDO; ENDDO; ENDDO
 CLOSE(JU)

 END SUBROUTINE STOMP_WRITE_HYDRAULICPROPERTIES

 !###======================================================================
 SUBROUTINE STOMP_WRITE_INITIAL_CONDITIONS(IU,ITYPE)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),PARAMETER :: RHO=1000.0
 REAL(KIND=DP_KIND),PARAMETER :: G=9.8
 REAL(KIND=DP_KIND),PARAMETER :: PAIR=101325.0
 INTEGER,INTENT(IN) :: IU,ITYPE
 INTEGER :: JU,IROW,ICOL,ILAY,JLAY
 REAL(KIND=DP_KIND) :: HB,WP,T,GP,H
 LOGICAL :: LWCONSTANT,LGCONSTANT
 
 LWCONSTANT=.TRUE.
 LGCONSTANT=.FALSE.
 
 WRITE(IU,'(A)') ''
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') '~Initial Conditions Card'
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') 'Aqueous Pressure,Gas Pressure,'
 WRITE(IU,'(A)') '2,'
 WRITE(IU,'(A)') 'Aqueous Pressure File,,Pa,ini_aqueous.dat,'
 WRITE(IU,'(A)') 'Gas Pressure File,,Pa,ini_gas.dat,'

 !## relation with depth
 JU=UTL_GETUNIT(); OPEN(JU,FILE=TRIM(OUTPUTFILE)//'\ini_aqueous.dat',STATUS='UNKNOWN')
 JLAY=0
 DO ILAY=ILAY1,ILAY2,DLAY; JLAY=JLAY+1; DO IROW=1,NROW; DO ICOL=1,NCOL
  IF(LWCONSTANT)THEN

!   WP=MAX(0.2,WAT(ILAY)%X(ICOL,IROW))

   !## initial pressure assigned to centroid of cell
   T =(TB(ILAY,1)%X(ICOL,IROW)+TB(ILAY,2)%X(ICOL,IROW))/2.0
   H = HED(ILAY)%X(ICOL,IROW)
   WP=(H-T)*RHO*G+PAIR
   
  ELSE
   !## initial pressure assigned to centroid of cell
   T=TB(ILAY,1)%X(ICOL,IROW)-TB(ILAY,2)%X(ICOL,IROW)  
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
   GP=101325.0D0+1.0D-9
  ELSE

   !## water pressure
   T =(TB(ILAY,1)%X(ICOL,IROW)+TB(ILAY,2)%X(ICOL,IROW))/2.0
   H = HED(ILAY)%X(ICOL,IROW)
   WP=(H-T)*RHO*G+PAIR
   GP=WP
   
   IF(GASPOINTER%X(ICOL,IROW).EQ.1)THEN
   
    SELECT CASE (ITYPE)
     CASE (1,3)
      !## add 10 meter gascolumn
      GP=WP +96000.0D0
     CASE (2,4)
      !## add 230 meter gascolumn
      GP=WP+2240000.0D0
    END SELECT

   !## initial pressure assigned to centroid of cell
   T=TB(ILAY,1)%X(ICOL,IROW)-TB(ILAY,2)%X(ICOL,IROW)  
   !## get total depth up to mid of current modellayer
   T =(TB(ILAY,2)%X(ICOL,IROW)-TB(ILAY1,2)%X(ICOL,IROW))+0.5*T
   HB=T

   SELECT CASE (ITYPE)
    CASE (3,4)
     GP=GP-(205.0D0*HB)
   END SELECT
  
  ENDIF

if(gp.le.0)then
write(*,*) gp.lt.0; pause; stop
endif
   
   IF(JLAY.EQ.NLAY)GP=WP
   
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
  WRITE(JU,'(F15.3)') 5.0
 ENDDO; ENDDO; ENDDO
 CLOSE(JU)
 JU=UTL_GETUNIT(); OPEN(JU,FILE=TRIM(OUTPUTFILE)//'\gsh.dat',STATUS='UNKNOWN')
 DO ILAY=ILAY1,ILAY2,DLAY; DO IROW=1,NROW; DO ICOL=1,NCOL
  WRITE(JU,'(F15.3)') 775.0
 ENDDO; ENDDO; ENDDO
 CLOSE(JU)

 END SUBROUTINE STOMP_WRITE_THERMAL_PROPERTY

 !###======================================================================
 SUBROUTINE STOMP_WRITE_SOURCES(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: N,IROW,ICOL,ILAY,JLAY
 REAL(KIND=DP_KIND) :: QF
 REAL(KIND=DP_KIND),DIMENSION(2) :: QT

 WRITE(IU,'(A)') ''
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') '~Source Card'
 WRITE(IU,'(A)') '#-------------------------------------------------------'

 N=0
 DO ILAY=ILAY1,ILAY2,DLAY
  DO IROW=1,NROW; DO ICOL=1,NCOL
   IF(Q(ILAY)%X(ICOL,IROW).NE.0.0)N=N+1
  ENDDO; ENDDO
 ENDDO
 
 WRITE(IU,'(A)') TRIM(ITOS(N))//','
 
 QT=0.0; JLAY=0; DO ILAY=ILAY1,ILAY2,DLAY; JLAY=JLAY+1
  DO IROW=1,NROW; DO ICOL=1,NCOL
   IF(Q(ILAY)%X(ICOL,IROW).NE.0.0)THEN
    WRITE(IU,'(A)') 'Aqueous Volumetric,null,null,'//TRIM(ITOS(ICOL))//','//TRIM(ITOS(ICOL))//','//TRIM(ITOS(IROW))//','// &
        TRIM(ITOS(IROW))//','//TRIM(ITOS(JLAY))//','//TRIM(ITOS(JLAY))//',1,'
    QF=Q(ILAY)%X(ICOL,IROW)/86400.0
    if(qf.gt.0.0)then
     QT(1)=QT(1)+QF
    else
     QT(2)=QT(2)+QF
    endif
    WRITE(IU,'(A)') '0,yr,3265,psi,'//TRIM(RTOS(QF,'G',7))//',m^3/s,'
   ENDIF
  ENDDO; ENDDO
 ENDDO
 WRITE(*,*) QT,QT*86400.0D0
 
 END SUBROUTINE STOMP_WRITE_SOURCES

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
 WRITE(IU,'(A)') '20,'
 WRITE(IU,'(A)') '0,s,'
 WRITE(IU,'(A)') '5,s,'
 WRITE(IU,'(A)') '60,s,'
 WRITE(IU,'(A)') '1,hour,'
 WRITE(IU,'(A)') '85,min,'
 WRITE(IU,'(A)') '1,day,'
 WRITE(IU,'(A)') '10,day,'
 WRITE(IU,'(A)') '50,day,'
 WRITE(IU,'(A)') '100,day,'
 WRITE(IU,'(A)') '200,day,'
 WRITE(IU,'(A)') '1,year,'
 WRITE(IU,'(A)') '2,year,'
 WRITE(IU,'(A)') '5,year,'
 WRITE(IU,'(A)') '10,year,'
 WRITE(IU,'(A)') '20,year,'
 WRITE(IU,'(A)') '30,year,'
 WRITE(IU,'(A)') '50,year,'
 WRITE(IU,'(A)') '100,year,'
 WRITE(IU,'(A)') '200,year,'
 WRITE(IU,'(A)') '300,year,'
! WRITE(IU,'(A)') '2,'         !## number of plot files
! WRITE(IU,'(A)') '1,year,'   !## number of plot files
! WRITE(IU,'(A)') '10,year,'  !## number of plot files
 WRITE(IU,'(A)') '8,'         !## number of output variables
 WRITE(IU,'(A)') 'aqueous saturation,,'        
 WRITE(IU,'(A)') 'aqueous pressure,,' !## aqueous pressure
 WRITE(IU,'(A)') 'gas saturation,,'            
 WRITE(IU,'(A)') 'gas pressure,Pa,'
 WRITE(IU,'(A)') 'aqueous relative perm,,' !## aqueous relative permeability
 WRITE(IU,'(A)') 'trapped gas sat,,'  !## trapped gas saturation,'
 WRITE(IU,'(A)') 'x aqueous volumetric flux,,' !## aqueous relative permeability
 WRITE(IU,'(A)') 'x gas volumetric flux,,'  !## trapped gas saturation,'

 END SUBROUTINE STOMP_WRITE_OUTPUT
 
 !###======================================================================
 SUBROUTINE STOMP_WRITE_GRID(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 INTEGER :: JU,KU,I,J,K,N,IROW,ICOL,ILAY,E,IR,IC
 REAL(KIND=DP_KIND),DIMENSION(2) :: X,Y
 REAL(KIND=DP_KIND),DIMENSION(8) :: Z
 INTEGER,DIMENSION(8) :: IX,IY,IZ
 REAL(KIND=DP_KIND) :: XC,YC,ZC,NZ
 DATA IX/1,2,1,2,1,2,1,2/
 DATA IY/2,2,1,1,2,2,1,1/
 DATA IZ/1,2,4,3,5,6,8,7/

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
 N=0; E=0; DO ILAY=ILAY1,ILAY2,DLAY; DO IROW=1,NROW; DO ICOL=1,NCOL

  X(1)=BND(1)%SX(ICOL-1); X(2)=BND(1)%SX(ICOL)
  Y(1)=BND(1)%SY(IROW-1); Y(2)=BND(1)%SY(IROW)

  Z =0.0

  K=-4
  DO J=2,1,-1
   K=K+4
   
   I=1; NZ=0.0
   DO IR=IROW,MIN(IROW+1,NROW)
    DO IC=MAX(ICOL-1,1),ICOL
     IF(TB(ILAY,J)%X(IC,IR).EQ.TB(ILAY,J)%NODATA)CYCLE
     NZ=NZ+1.0; Z(I+K)=Z(I+K)+TB(ILAY,J)%X(IC,IR)
    ENDDO
   ENDDO
   Z(I+K)=Z(I+K)/NZ
      
   I=2; NZ=0.0
   DO IR=IROW,MIN(IROW+1,NROW)
    DO IC=ICOL,MIN(ICOL+1,NCOL)
     IF(TB(ILAY,J)%X(IC,IR).EQ.TB(ILAY,J)%NODATA)CYCLE
     NZ=NZ+1.0; Z(I+K)=Z(I+K)+TB(ILAY,J)%X(IC,IR)
    ENDDO
   ENDDO
   Z(I+K)=Z(I+K)/NZ

   I=3; NZ=0.0
   DO IR=MAX(IROW-1,1),IROW
    DO IC=ICOL,MIN(ICOL+1,NCOL)
     IF(TB(ILAY,J)%X(IC,IR).EQ.TB(ILAY,J)%NODATA)CYCLE
     NZ=NZ+1.0; Z(I+K)=Z(I+K)+TB(ILAY,J)%X(IC,IR)
    ENDDO
   ENDDO
   Z(I+K)=Z(I+K)/NZ

   I=4; NZ=0.0
   DO IR=MAX(IROW-1,1),IROW
    DO IC=MAX(ICOL-1,1),ICOL
     IF(TB(ILAY,J)%X(IC,IR).EQ.TB(ILAY,J)%NODATA)CYCLE
     NZ=NZ+1.0; Z(I+K)=Z(I+K)+TB(ILAY,J)%X(IC,IR)
    ENDDO
   ENDDO
   Z(I+K)=Z(I+K)/NZ
 
   DO I=1,4
    IF(Z(I+K).EQ.TB(ILAY,J)%NODATA)Z(I+K)=-999.99
   ENDDO

  ENDDO

  E=E+1
  WRITE(KU,'(9I10)') E,(K,K=N+1,N+8)

  DO J=1,8

   XC=X(IX(J))
   YC=Y(IY(J))
   ZC=Z(IZ(J))

   N=N+1
   WRITE(JU,'(I10,3(A1,F10.2),A1)') N,',',XC,',',YC,',',ZC,','

  ENDDO

 ENDDO; ENDDO; ENDDO

 CLOSE(JU); CLOSE(KU)

 END SUBROUTINE STOMP_WRITE_GRID

 !###======================================================================
 LOGICAL FUNCTION STOMP_READ(IU,IDF,IWINDOW,ICROSS,SEAL)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 INTEGER,INTENT(IN) :: IWINDOW,IU,ICROSS
 REAL(KIND=DP_KIND),INTENT(IN) :: SEAL
 INTEGER :: I,J,K,II,ILAY,JLAY,KLAY,IROW,ICOL
 REAL(KIND=DP_KIND) :: K1,K2,K3,K4,T,B,D,DX,DY,DXY,A,T1,T2,QF,CC,DH
 CHARACTER(LEN=256) :: LINE
 REAL(KIND=DP_KIND),DIMENSION(4) :: QT
 
 STOMP_READ=.FALSE.

 NLAY=SIZE(BND)

 IF(IWINDOW.EQ.1)CALL UTL_IDFSNAPTOGRID_LLC(IDF%XMIN,IDF%XMAX,IDF%YMIN,IDF%YMAX,IDF%DX,IDF%DY,IDF%NCOL,IDF%NROW,.TRUE.)
 IF(ICROSS.EQ.1)THEN
  DX=(IDF%XMAX-IDF%XMIN); DY=(IDF%YMAX-IDF%YMIN); DXY=DX**2.0D0+DY**2.0D0; IF(DXY.GT.0.0)DXY=SQRT(DXY)
  IDF%NROW=1; IDF%NCOL=DXY/IDF%DX; A=ATAN(DY/DX); DXY=IDF%NCOL*IDF%DX
  !## get upper corner
  IDF%XMAX=IDF%XMIN+COS(A)*DXY; IDF%YMAX=IDF%YMIN+SIN(A)*DXY
 ENDIF
 
 IF(.NOT.UTL_READINITFILE('SURFLEVEL',LINE,IU,0))RETURN
 READ(LINE,*) SLEVEL%FNAME; LINE='SURFLEVEL='//TRIM(SLEVEL%FNAME); WRITE(*,'(A)') TRIM(LINE)
 IF(.NOT.UTL_READINITFILE('GASPOINTER',LINE,IU,0))RETURN
 READ(LINE,*) GASPOINTER%FNAME; LINE='GASPOINTER='//TRIM(GASPOINTER%FNAME); WRITE(*,'(A)') TRIM(LINE)

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
  IF(.NOT.UTL_READINITFILE('HED_L'//TRIM(ITOS(I)),LINE,IU,0))RETURN
  READ(LINE,*) HED(I)%FNAME; LINE='HED_L'//TRIM(ITOS(I))//'='//TRIM(HED(I)%FNAME); WRITE(*,'(A)') TRIM(LINE)
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
  SLEVEL%FNAME=TRIM(OUTPUTFILE)//'\INPUT_IDF\SLEVEL.IDF'
  IF(.NOT.IDFWRITE(SLEVEL,SLEVEL%FNAME,1))RETURN

  CALL IDFCOPY(IDF,GASPOINTER)
  IF(.NOT.IDFREADCROSS(GASPOINTER,GASPOINTER%FNAME))RETURN
  GASPOINTER%FNAME=TRIM(OUTPUTFILE)//'\INPUT_IDF\GASPOINTER.IDF'
  IF(.NOT.IDFWRITE(GASPOINTER,GASPOINTER%FNAME,1))RETURN

  K=0
  DO I=1,NLAY
   CALL IDFCOPY(IDF,BND(I))
   IF(.NOT.IDFREADCROSS(BND(I),BND(I)%FNAME))RETURN
   BND(I)%FNAME=TRIM(OUTPUTFILE)//'\INPUT_IDF\BND_L'//TRIM(ITOS(I))//'.IDF' 
   IF(NAGGR.EQ.0)THEN
    IF(.NOT.IDFWRITE(BND(I),BND(I)%FNAME,1))RETURN
   ENDIF
   K=K+1
   DO J=1,SIZE(TB,2)
    CALL IDFCOPY(IDF,TB(I,J))   
    IF(.NOT.IDFREADCROSS(TB(I,J),TB(I,J)%FNAME))RETURN
    IF(J.EQ.1)THEN
     TB(I,J)%FNAME=TRIM(OUTPUTFILE)//'\INPUT_IDF\TOP_L'//TRIM(ITOS(I))//'.IDF' 
    ELSE
     TB(I,J)%FNAME=TRIM(OUTPUTFILE)//'\INPUT_IDF\BOT_L'//TRIM(ITOS(I))//'.IDF' 
    ENDIF
    IF(NAGGR.EQ.0)THEN
     IF(.NOT.IDFWRITE(TB(I,J),TB(I,J)%FNAME,1))RETURN
    ENDIF
   ENDDO
   K=K+1
   DO J=1,SIZE(KHV,2)
    CALL IDFCOPY(IDF,KHV(I,J))   
    IF(.NOT.IDFREADCROSS(KHV(I,J),KHV(I,J)%FNAME))RETURN
    SELECT CASE (J)
     CASE (1); KHV(I,J)%FNAME=TRIM(OUTPUTFILE)//'\INPUT_IDF\KHX_L'//TRIM(ITOS(I))//'.IDF' 
     CASE (2); KHV(I,J)%FNAME=TRIM(OUTPUTFILE)//'\INPUT_IDF\KHY_L'//TRIM(ITOS(I))//'.IDF' 
     CASE (3); KHV(I,J)%FNAME=TRIM(OUTPUTFILE)//'\INPUT_IDF\KHZ_L'//TRIM(ITOS(I))//'.IDF' 
    END SELECT

    IF(NAGGR.EQ.0)THEN
     IF(.NOT.IDFWRITE(KHV(I,J),KHV(I,J)%FNAME,1))RETURN
    ENDIF
   ENDDO

   CALL IDFCOPY(IDF,WAT(I))
   IF(.NOT.IDFREADCROSS(WAT(I),WAT(I)%FNAME))RETURN
   WAT(I)%FNAME=TRIM(OUTPUTFILE)//'\INPUT_IDF\WAT_L'//TRIM(ITOS(I))//'.IDF'
   IF(NAGGR.EQ.0)THEN
    IF(.NOT.IDFWRITE(WAT(I),WAT(I)%FNAME,1))RETURN
   ENDIF
   
   CALL IDFCOPY(IDF,HED(I))
   IF(.NOT.IDFREADCROSS(HED(I),HED(I)%FNAME))RETURN
   HED(I)%FNAME=TRIM(OUTPUTFILE)//'\INPUT_IDF\HED_L'//TRIM(ITOS(I))//'.IDF'
   IF(NAGGR.EQ.0)THEN
    IF(.NOT.IDFWRITE(HED(I),HED(I)%FNAME,1))RETURN
   ENDIF

  ENDDO
 
 ELSE

  IF(.NOT.IDFREADCROSS(IDF,IDF%FNAME))RETURN
  IF(I.EQ.1.AND.IWINDOW.EQ.0)THEN
   IF(.NOT.IDFREAD(IDF,IDF%FNAME,1))RETURN
  ELSE
   IF(.NOT.IDFREADSCALE(IDF%FNAME,IDF,1,1,0.0D0,0))RETURN
  ENDIF
!  IDF%FNAME=TRIM(OUTPUTFILE)//'\INPUT_IDF\'//TRIM(IDF%FNAME(INDEX(IDF%FNAME,'\',.TRUE.):))
!  IF(.NOT.IDFWRITE(IDF,IDF%FNAME,1))RETURN

  DO I=1,NLAY
   WRITE(*,'(A,I10)') 'Reading files for layer ',I
   IF(I.EQ.1.AND.IWINDOW.EQ.0)THEN
    IF(.NOT.IDFREAD(BND(I),BND(I)%FNAME,1))RETURN
    CALL IDFCOPY(BND(I),IDF)
   ELSE
    CALL IDFCOPY(IDF,BND(I)); IF(.NOT.IDFREADSCALE(BND(I)%FNAME,BND(I),1,1,0.0D0,0))RETURN
   ENDIF
   !## reading top/bot as average values
   DO J=1,SIZE(TB,2)
    CALL IDFCOPY(BND(I),TB(I,J))
    IF(.NOT.IDFREADSCALE(TB(I,J)%FNAME,TB(I,J),2,1,0.0D0,0))RETURN
   ENDDO
   !## reading permeability as geometric means
   DO J=1,SIZE(KHV,2)
    CALL IDFCOPY(BND(I),KHV(I,J))
    IF(.NOT.IDFREADSCALE(KHV(I,J)%FNAME,KHV(I,J)  ,3,1,0.0D0,0))RETURN
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

 !## if vertical aggregrate - do that
 IF(NAGGR.GT.0)THEN

  !## quick and dirty
  DO IROW=1,IDF%NROW; DO ICOL=1,IDF%NCOL
   IF(BND(1)%X(ICOL,IROW).EQ.0)CYCLE
   TB(1,1)%X(ICOL,IROW)=TB(2,1)%X(ICOL,IROW)+5.0
   !## insert seal strength
   KHV(1,1)%X(ICOL,IROW)=SEAL
   KHV(1,2)%X(ICOL,IROW)=SEAL
   KHV(1,3)%X(ICOL,IROW)=SEAL
  ENDDO; ENDDO

  ILAY=0; JLAY=0
  DO I=1,NAGGR
   JLAY=JLAY+1

   !## scaling boundary
   DO IROW=1,IDF%NROW; DO ICOL=1,IDF%NCOL; DO J=1,IAGGR(I)
    KLAY=ILAY+J
    IF(BND(KLAY)%X(ICOL,IROW).EQ.0)THEN
     BND(JLAY)%X(ICOL,IROW)=0.0
    ELSE
     BND(JLAY)%X(ICOL,IROW)=1.0
    ENDIF
   ENDDO; ENDDO; ENDDO

   !## scaling top/bot
   DO IROW=1,IDF%NROW; DO ICOL=1,IDF%NCOL
    IF(BND(JLAY)%X(ICOL,IROW).EQ.0)THEN
     TB(JLAY,1)%X(ICOL,IROW)=TB(JLAY,1)%NODATA
     TB(JLAY,2)%X(ICOL,IROW)=TB(JLAY,2)%NODATA
    ELSE  
     DO J=1,IAGGR(I)
      KLAY=ILAY+J
      IF(J.EQ.1)THEN
       TB(JLAY,1)%X(ICOL,IROW)=TB(KLAY,1)%X(ICOL,IROW)
       TB(JLAY,2)%X(ICOL,IROW)=TB(KLAY,2)%X(ICOL,IROW)
      ELSE
       TB(JLAY,1)%X(ICOL,IROW)=MAX(TB(JLAY,1)%X(ICOL,IROW),TB(KLAY,1)%X(ICOL,IROW))
       TB(JLAY,2)%X(ICOL,IROW)=MIN(TB(JLAY,2)%X(ICOL,IROW),TB(KLAY,2)%X(ICOL,IROW))
      ENDIF
     ENDDO
    ENDIF
   ENDDO; ENDDO
   
   !## scaling water saturation
   DO IROW=1,IDF%NROW; DO ICOL=1,IDF%NCOL
    IF(BND(JLAY)%X(ICOL,IROW).EQ.0)THEN
     WAT(JLAY)%X(ICOL,IROW)=WAT(JLAY)%NODATA
     HED(JLAY)%X(ICOL,IROW)=HED(JLAY)%NODATA
    ELSE  
     DO J=1,IAGGR(I)
      KLAY=ILAY+J
      IF(J.EQ.1)THEN
       WAT(JLAY)%X(ICOL,IROW)=WAT(KLAY)%X(ICOL,IROW)
       HED(JLAY)%X(ICOL,IROW)=HED(KLAY)%X(ICOL,IROW)
      ELSE
       WAT(JLAY)%X(ICOL,IROW)=WAT(JLAY)%X(ICOL,IROW)+WAT(KLAY)%X(ICOL,IROW)
       HED(JLAY)%X(ICOL,IROW)=HED(JLAY)%X(ICOL,IROW)+HED(KLAY)%X(ICOL,IROW)
      ENDIF
     ENDDO
     WAT(JLAY)%X(ICOL,IROW)=WAT(JLAY)%X(ICOL,IROW)/REAL(IAGGR(I),8)
     HED(JLAY)%X(ICOL,IROW)=HED(JLAY)%X(ICOL,IROW)/REAL(IAGGR(I),8)
    ENDIF
   ENDDO; ENDDO

   !## scaling top/bot
   DO IROW=1,IDF%NROW; DO ICOL=1,IDF%NCOL
    IF(BND(JLAY)%X(ICOL,IROW).EQ.0)THEN
     KHV(JLAY,1)%X(ICOL,IROW)=KHV(JLAY,1)%NODATA
     KHV(JLAY,2)%X(ICOL,IROW)=KHV(JLAY,2)%NODATA
     KHV(JLAY,3)%X(ICOL,IROW)=KHV(JLAY,3)%NODATA
    ELSE  
     DO J=1,IAGGR(I)
      KLAY=ILAY+J
      IF(J.EQ.1)THEN
       KHV(JLAY,1)%X(ICOL,IROW)= LOG(KHV(KLAY,1)%X(ICOL,IROW))
       KHV(JLAY,2)%X(ICOL,IROW)= LOG(KHV(KLAY,2)%X(ICOL,IROW))
       KHV(JLAY,3)%X(ICOL,IROW)=1.0/(KHV(KLAY,3)%X(ICOL,IROW))
      ELSE
       KHV(JLAY,1)%X(ICOL,IROW)= LOG(KHV(KLAY,1)%X(ICOL,IROW))+KHV(JLAY,1)%X(ICOL,IROW)
       KHV(JLAY,2)%X(ICOL,IROW)= LOG(KHV(KLAY,2)%X(ICOL,IROW))+KHV(JLAY,2)%X(ICOL,IROW)
       KHV(JLAY,3)%X(ICOL,IROW)=1.0/(KHV(KLAY,3)%X(ICOL,IROW))+KHV(JLAY,3)%X(ICOL,IROW)
      ENDIF
     ENDDO
     KHV(JLAY,1)%X(ICOL,IROW)=  EXP(KHV(JLAY,1)%X(ICOL,IROW)/REAL(IAGGR(I),8))
     KHV(JLAY,2)%X(ICOL,IROW)=  EXP(KHV(JLAY,2)%X(ICOL,IROW)/REAL(IAGGR(I),8))
     KHV(JLAY,3)%X(ICOL,IROW)= 1.0/(KHV(JLAY,3)%X(ICOL,IROW)/REAL(IAGGR(I),8))    
    ENDIF
   ENDDO; ENDDO

   IF(.NOT.IDFWRITE(BND(JLAY),BND(JLAY)%FNAME,1))RETURN
   DO J=1,SIZE(TB,2)
    IF(.NOT.IDFWRITE(TB(JLAY,J),TB(JLAY,J)%FNAME,1))RETURN
   ENDDO
   DO J=1,SIZE(KHV,2)
    IF(.NOT.IDFWRITE(KHV(JLAY,J),KHV(JLAY,J)%FNAME,1))RETURN
   ENDDO
   IF(.NOT.IDFWRITE(WAT(JLAY),WAT(JLAY)%FNAME,1))RETURN
   IF(.NOT.IDFWRITE(HED(JLAY),HED(JLAY)%FNAME,1))RETURN

   ILAY=ILAY+IAGGR(I)
  
  ENDDO
  
  !## new number of layers
  NLAY=JLAY
 
 ENDIF
 
 QT=0.0
 DO JLAY=1,NLAY
  CALL IDFCOPY(IDF,Q(JLAY))
  !## compute fluxes (estimated)
  DO IROW=1,BND(1)%NROW; DO ICOL=1,BND(1)%NCOL
   IF(HED(JLAY)%X(ICOL,IROW).EQ.HED(JLAY)%NODATA)CYCLE
   QF=0.0
   IF(JLAY.EQ.1)THEN
    DH= HED(JLAY)%X(ICOL,IROW)-HED(JLAY+1)%X(ICOL,IROW)
    D = 0.5*(TB(JLAY,1)%X(ICOL,IROW)-TB(JLAY+1,2)%X(ICOL,IROW))
    CC=(HED(JLAY)%DX*HED(JLAY)%DX)/(D/KHV(JLAY,3)%X(ICOL,IROW))
    QF= DH*CC
    QT(1)=QT(1)+QF
   ELSEIF(JLAY.EQ.NLAY)THEN
    DH= HED(JLAY)%X(ICOL,IROW)-HED(JLAY-1)%X(ICOL,IROW)
    D = 0.5*(TB(JLAY-1,1)%X(ICOL,IROW)-TB(JLAY,2)%X(ICOL,IROW))
    CC=(HED(JLAY)%DX*HED(JLAY)%DX)/(D/KHV(JLAY,3)%X(ICOL,IROW))
    QF=DH*CC
    QT(2)=QT(2)+QF
   ELSE
    IF(ICOL.EQ.1)THEN
     DH= HED(JLAY)%X(ICOL,IROW)-HED(JLAY)%X(ICOL+1,IROW)
     T1= KHV(JLAY,1)%X(ICOL  ,IROW)*(TB(JLAY,1)%X(ICOL  ,IROW)-TB(JLAY,2)%X(ICOL  ,IROW))
     T2= KHV(JLAY,1)%X(ICOL+1,IROW)*(TB(JLAY,1)%X(ICOL+1,IROW)-TB(JLAY,2)%X(ICOL+1,IROW))
     CC=(2.0*T1*T2*HED(JLAY)%DX)/(T1*HED(JLAY)%DX+T2*HED(JLAY)%DX)
     QF= DH*CC
     QT(3)=QT(3)+QF
    ELSEIF(ICOL.EQ.BND(1)%NCOL)THEN
     DH= HED(JLAY)%X(ICOL,IROW)-HED(JLAY)%X(ICOL-1,IROW)
     T1= KHV(JLAY,1)%X(ICOL-1,IROW)*(TB(JLAY,1)%X(ICOL-1,IROW)-TB(JLAY,2)%X(ICOL-1,IROW))
     T2= KHV(JLAY,1)%X(ICOL  ,IROW)*(TB(JLAY,1)%X(ICOL  ,IROW)-TB(JLAY,2)%X(ICOL  ,IROW))
     CC=(2.0*T1*T2*HED(JLAY)%DX)/(T1*HED(JLAY)%DX+T2*HED(JLAY)%DX)
     QF= DH*CC
     QT(4)=QT(4)+QF    
    ENDIF
   ENDIF
   Q(JLAY)%X(ICOL,IROW)=QF
  ENDDO; ENDDO
 ENDDO
 
 WRITE(*,*) QT

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
 IF(ALLOCATED(HED))THEN
  CALL IDFDEALLOCATE(HED,SIZE(HED)); DEALLOCATE(HED)
 ENDIF
 IF(ALLOCATED(WAT))THEN
  CALL IDFDEALLOCATE(WAT,SIZE(WAT)); DEALLOCATE(WAT)
 ENDIF
 IF(ALLOCATED(Q))THEN
  CALL IDFDEALLOCATE(Q,SIZE(Q)); DEALLOCATE(Q)
 ENDIF

 END SUBROUTINE STOMP_CLOSE

 !###======================================================================
 SUBROUTINE STOMP_INIT(NLAY)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NLAY
 INTEGER :: I,J

 ALLOCATE(TB(NLAY,2),KHV(NLAY,3),BND(NLAY),WAT(NLAY),HED(NLAY),Q(NLAY))
 DO I=1,NLAY                    ; CALL IDFNULLIFY(BND(I)) ; ENDDO
 DO I=1,NLAY                    ; CALL IDFNULLIFY(WAT(I)) ; ENDDO
 DO I=1,NLAY                    ; CALL IDFNULLIFY(HED(I)) ; ENDDO
 DO I=1,NLAY                    ; CALL IDFNULLIFY(Q(I))   ; ENDDO
 DO I=1,NLAY; DO J=1,SIZE(TB,2) ; CALL IDFNULLIFY(TB(I,J)); ENDDO; ENDDO
 DO I=1,NLAY; DO J=1,SIZE(KHV,2); CALL IDFNULLIFY(KHV(I,J)) ; ENDDO; ENDDO
 CALL IDFNULLIFY(SLEVEL)
 
 END SUBROUTINE STOMP_INIT

 !###======================================================================
 REAL(KIND=DP_KIND) FUNCTION STOMP_GETPOROSITY(K)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),PARAMETER :: C1=0.018
 REAL(KIND=DP_KIND),PARAMETER :: C2=0.2958
 REAL(KIND=DP_KIND),INTENT(INOUT) :: K
 REAL(KIND=DP_KIND) :: P
 
 IF(K.EQ.0.0D0)K=TINY(1.0D0)
 K=MAX(K,0.000000086D0)

 P=C1*LOG(K)+C2
  
 IF(P.LT.0.0D0)THEN
 WRITE(*,*)
 ENDIF
 
 STOMP_GETPOROSITY=P
 
 END FUNCTION STOMP_GETPOROSITY
 
END MODULE MOD_STOMP
