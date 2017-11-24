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
USE MOD_IDF, ONLY : IDFDEALLOCATE,IDFCOPY,IDFREADSCALE,IDFREAD,IDFNULLIFY,UTL_WRITE_FREE_ROW,IDFFILLSXSY
USE MOD_UTL, ONLY : UTL_IDFSNAPTOGRID_LLC,UTL_READINITFILE,ITOS,UTL_GETUNIT,UTL_CREATEDIR,RTOS,ITOS
USE MOD_OSD, ONLY : OSD_GETENV,OSD_DATE_AND_TIME
CHARACTER(LEN=256),PRIVATE :: LINE

CONTAINS

 !###======================================================================
 LOGICAL FUNCTION STOMP_SAVEINPUT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IU
 
 STOMP_SAVEINPUT=.FALSE.
 
 IU=UTL_GETUNIT(); OPEN(IU,FILE=TRIM(OUTPUTFILE)//'\input',STATUS='UNKNOWN')
 
 CALL STOMP_WRITE_SIM_TITLE(IU)
 CALL STOMP_WRITE_SOL_CNTRL(IU)
 CALL STOMP_WRITE_GRID(IU)
 CALL STOMP_WRITE_INACTIVE(IU)
 CALL STOMP_WRITE_SOILZONATION(IU)
 CALL STOMP_WRITE_MECHANICAL(IU)
 CALL STOMP_WRITE_HYDRAULICPROPERTIES(IU)
 CALL STOMP_WRITE_SAT_FUNC(IU)
 CALL STOMP_WRITE_AQ_RE_PERM(IU)
 CALL STOMP_WRITE_GAS_REL_PERM(IU)
 
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
 WRITE(IU,'(A)') 'kISMET Well Temperatures,'       !## simulation title
 WRITE(IU,'(A)') TRIM(OSD_GETENV('USERNAME'))//',' !## username
 WRITE(IU,'(A)') 'DELTARES,'                       !## company name
 CALL OSD_DATE_AND_TIME(DATEANDTIME=DATESTRING) 
 WRITE(IU,'(A)') DATESTRING(:11)//','              !## input creation date
 WRITE(IU,'(A)') DATESTRING(13:20)//','            !## input creation time
 WRITE(IU,'(A)') '0,'                              !## number of simulation note lines
! WRITE(IU,'(A)') 'Model of the kISMET Well Temperatures' !## simulation note

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
 WRITE(IU,'(A)') '10000,'
 WRITE(IU,'(A)') 'Variable Aqueous Diffusion,'
 WRITE(IU,'(A)') 'Variable Gas Diffusion,'
 WRITE(IU,'(A)') '0,'

 END SUBROUTINE STOMP_WRITE_SOL_CNTRL

 !###======================================================================
 SUBROUTINE STOMP_WRITE_SAT_FUNC(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU 

 WRITE(IU,'(A)') ''
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') '~Saturation Function Card'
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') 'Poorman,van Genuchten w/Webb,0.186,1/m,1.529,0.06,0.346,,'
 
 END SUBROUTINE STOMP_WRITE_SAT_FUNC
 
 !###======================================================================
 SUBROUTINE STOMP_WRITE_AQ_RE_PERM(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU 

 WRITE(IU,'(A)') ''
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') '~Aqueous Relative Permeability Card'
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') 'Poorman,Mualem,,'

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
 WRITE(IU,'(A)') 'Poorman,Mualem,,'
 
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
 WRITE(IU,'(A)') 'File,inactive.dat'

 JU=UTL_GETUNIT(); OPEN(JU,FILE=TRIM(OUTPUTFILE)//'\inactive.dat',STATUS='UNKNOWN')
 DO ILAY=1,NLAY; DO IROW=1,NROW; DO ICOL=1,NCOL
  IF(BND(ILAY)%X(ICOL,IROW).EQ.0)THEN
   WRITE(JU,'(3I10)') ICOL,IROW,ILAY
  ENDIF
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
 
 WRITE(IU,'(A)') ''
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') '~Mechanical Properties Card'
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') 'IJK Indexing,2690,kg/m^3,file:por.dat,file:por.dat,,1/m,Millington and Quirk,'
 
! JU=UTL_GETUNIT(); OPEN(JU,FILE=TRIM(OUTPUTFILE)//'\inactive.dat',STATUS='UNKNOWN')
! DO ILAY=1,NLAY; DO IROW=1,NROW; DO ICOL=1,NCOL
!  IF(BND(ILAY)%X(ICOL,IROW).EQ.0)THEN
!   WRITE(JU,'(3I10)') ICOL,IROW,ILAY
!  ENDIF
! ENDDO; ENDDO; ENDDO
! CLOSE(JU)

 END SUBROUTINE STOMP_WRITE_MECHANICAL
 
 !###======================================================================
 SUBROUTINE STOMP_WRITE_HYDRAULICPROPERTIES(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU 
 INTEGER :: JU,IROW,ICOL,ILAY
 REAL :: K1,K2,K3,K4
 
 WRITE(IU,'(A)') ''
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') '~Hydraulic Properties Card'
 WRITE(IU,'(A)') '#-------------------------------------------------------'
 WRITE(IU,'(A)') 'IJK Indexing, file:ksx.dat,m^2/d, file:ksy.dat,m^2/d, file:ksz.dat,m^2/d,'
 
 JU=UTL_GETUNIT(); OPEN(JU,FILE=TRIM(OUTPUTFILE)//'\ksx.dat',STATUS='UNKNOWN')
 DO ILAY=1,NLAY; DO IROW=1,NROW; DO ICOL=1,NCOL
  IF(BND(ILAY)%X(ICOL,IROW).EQ.0)CYCLE
  WRITE(JU,'(3I10,G15.7)') ICOL,IROW,ILAY,KHV(ILAY,1)%X(ICOL,IROW)
 ENDDO; ENDDO; ENDDO
 CLOSE(JU)
 
 JU=UTL_GETUNIT(); OPEN(JU,FILE=TRIM(OUTPUTFILE)//'\ksy.dat',STATUS='UNKNOWN')
 DO ILAY=1,NLAY; DO IROW=1,NROW; DO ICOL=1,NCOL
  IF(BND(ILAY)%X(ICOL,IROW).EQ.0)CYCLE
  WRITE(JU,'(3I10,G15.7)') ICOL,IROW,ILAY,KHV(ILAY,2)%X(ICOL,IROW)
 ENDDO; ENDDO; ENDDO
 CLOSE(JU)

 JU=UTL_GETUNIT(); OPEN(JU,FILE=TRIM(OUTPUTFILE)//'\ksz.dat',STATUS='UNKNOWN')
 DO ILAY=1,NLAY; DO IROW=1,NROW; DO ICOL=1,NCOL
  IF(BND(ILAY)%X(ICOL,IROW).EQ.0)CYCLE
  K1=LOG(KHV(ILAY,1)%X(ICOL,IROW))
  K2=LOG(KHV(ILAY,2)%X(ICOL,IROW))
  K3=EXP((K1+K2)/2.0)
  K4=K3*KHV(ILAY,3)%X(ICOL,IROW)
  WRITE(JU,'(3I10,G15.7)') ICOL,IROW,ILAY,K4
 ENDDO; ENDDO; ENDDO
 CLOSE(JU)

 END SUBROUTINE STOMP_WRITE_HYDRAULICPROPERTIES
 
 !###======================================================================
 SUBROUTINE STOMP_WRITE_GRID(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU 
 INTEGER :: JU,KU,I,J,K,IROW,ICOL,ILAY
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

! !## get number of nodes
! I=0; DO ILAY=1,NLAY; DO IROW=1,NROW; DO ICOL=1,NCOL
!  !## skip inactive nodes
!  IF(BND(ILAY)%X(ICOL,IROW).EQ.0)CYCLE
!  I=I+1
! ENDDO; ENDDO; ENDDO
 
 !## ncol,nrow,nlay
 LINE=TRIM(ITOS(NCOL))//','//TRIM(ITOS(NROW))//','//TRIM(ITOS(NLAY))//','; WRITE(IU,'(A)') TRIM(LINE)
 LINE='vertices.dat,'//TRIM(ITOS(NCOL*NROW*NLAY))//','; WRITE(IU,'(A)') TRIM(LINE)
 LINE='elements.dat,'; WRITE(IU,'(A)') TRIM(LINE)
 
 JU=UTL_GETUNIT(); OPEN(JU,FILE=TRIM(OUTPUTFILE)//'\vertices.dat',STATUS='UNKNOWN')
 KU=UTL_GETUNIT(); OPEN(KU,FILE=TRIM(OUTPUTFILE)//'\elements.dat',STATUS='UNKNOWN')

 IF(.NOT.IDFFILLSXSY(BND(1)))RETURN
 
 WRITE(JU,'(A)') 'x[m],y[m],z[m]' 
 
 !## 1 2 4 3 - bottom
 !## 5 6 8 7 - top
 I=0; DO ILAY=1,NLAY; DO IROW=1,NROW; DO ICOL=1,NCOL

!  !## skip inactive nodes
!  IF(BND(ILAY)%X(ICOL,IROW).EQ.0)CYCLE

  X(1)=BND(1)%SX(ICOL-1)
  X(2)=BND(1)%SX(ICOL  )
  Y(1)=BND(1)%SY(IROW-1)
  Y(2)=BND(1)%SY(IROW  )
  Z(2)=TB(ILAY,1)%X(ICOL,IROW) !## top
  Z(1)=TB(ILAY,2)%X(ICOL,IROW) !## bot
  
  IF(Z(2).EQ.TB(ILAY,1)%NODATA)Z(2)=-999.99
  IF(Z(1).EQ.TB(ILAY,2)%NODATA)Z(1)=-999.99
  
  WRITE(KU,'(8I10)') (K,K=I+1,I+8)

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
 LOGICAL FUNCTION STOMP_READ(IU,IDF,IWINDOW)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 INTEGER,INTENT(IN) :: IWINDOW,IU
 INTEGER :: I,J
 CHARACTER(LEN=256) :: LINE
 
 STOMP_READ=.FALSE.
 
 NLAY=SIZE(BND)
 
 IF(IWINDOW.EQ.1)CALL UTL_IDFSNAPTOGRID_LLC(IDF%XMIN,IDF%XMAX,IDF%YMIN,IDF%YMAX,IDF%DX,IDF%DY,IDF%NCOL,IDF%NROW,.TRUE.)

 DO I=1,NLAY
  IF(.NOT.UTL_READINITFILE('BND_L'//TRIM(ITOS(I)),LINE,IU,0))RETURN
  READ(LINE,*) BND(I)%FNAME; LINE='BND_L'//TRIM(ITOS(I))//'='//TRIM(BND(I)%FNAME); WRITE(*,'(A)') TRIM(LINE)
 ENDDO
 
 DO I=1,NLAY
  DO J=1,SIZE(KHV,2)
   IF(.NOT.UTL_READINITFILE(TRIM(TXT(J))//'_L'//TRIM(ITOS(I)),LINE,IU,0))RETURN
   READ(LINE,*) KHV(I,J)%FNAME; LINE=TRIM(TXT(J))//'_L'//TRIM(ITOS(I))//'='//TRIM(KHV(I,J)%FNAME); WRITE(*,'(A)') TRIM(LINE)
  ENDDO
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

 DO I=1,NLAY
  WRITE(*,'(A,I10)') 'Reading files for layer ',I
  IF(I.EQ.1.AND.IWINDOW.EQ.0)THEN
   IF(.NOT.IDFREAD(BND(I),BND(I)%FNAME,1))RETURN
   CALL IDFCOPY(BND(1),IDF)
  ELSE
   CALL IDFCOPY(IDF,BND(I)); IF(.NOT.IDFREADSCALE(BND(I)%FNAME,BND(I),1,1,0.0,0))RETURN
  ENDIF
  !## reading top/bot as average values
  DO J=1,SIZE(TB,2); CALL IDFCOPY(BND(I),TB(I,J)); IF(.NOT.IDFREADSCALE(TB(I,J)%FNAME,TB(I,J),2,1,0.0,0))RETURN; ENDDO
  !## reading permeability as geometric means
  DO J=1,SIZE(KHV,2);  CALL IDFCOPY(BND(I),KHV(I,J));  IF(.NOT.IDFREADSCALE(KHV(I,J)%FNAME,KHV(I,J)  ,3,1,0.0,0))RETURN; ENDDO
 ENDDO 

 NROW=IDF%NROW; NCOL=IDF%NCOL
 
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
 
 ALLOCATE(TB(NLAY,2),KHV(NLAY,3),BND(NLAY))
 DO I=1,NLAY                    ; CALL IDFNULLIFY(BND(I)) ; ENDDO
 DO I=1,NLAY; DO J=1,SIZE(TB,2) ; CALL IDFNULLIFY(TB(I,J)); ENDDO; ENDDO
 DO I=1,NLAY; DO J=1,SIZE(KHV,2); CALL IDFNULLIFY(KHV(I,J)) ; ENDDO; ENDDO

 END SUBROUTINE STOMP_INIT

END MODULE MOD_STOMP
