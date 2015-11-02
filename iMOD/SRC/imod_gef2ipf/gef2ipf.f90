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
MODULE MOD_GEF2IPF

USE WINTERACTER
USE MOD_GEF2IPF_PAR
USE MOD_UTL, ONLY : UTL_CREATEDIR,UTL_MESSAGEHANDLE

CONTAINS

 !###======================================================================
 SUBROUTINE GEF2IPF_MAIN(IBATCH)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH
 INTEGER :: I,ICOL,IROW,KU,IDIR,JCOL
 CHARACTER(LEN=256) :: LINE,IPFDIR
 
 IF(.NOT.ASSOCIATED(GEFNAMES))THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not find any files!','Error'); RETURN
 ENDIF

 IF(IBATCH.EQ.0)CALL UTL_MESSAGEHANDLE(0)

 I=INDEX(IPFFNAME,'\',.TRUE.)-1; IPFDIR=IPFFNAME(:I); CALL UTL_CREATEDIR(IPFDIR)
 JU=UTL_GETUNIT(); OPEN(JU,FILE=TRIM(IPFDIR)//IPFFNAME(I+1:),STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED')

 WRITE(JU,*) SIZE(GEFNAMES)
 WRITE(JU,*) 10
 WRITE(JU,*) 'X'
 WRITE(JU,*) 'Y'
 WRITE(JU,*) 'ID'
 WRITE(JU,*) 'Z_END'
 WRITE(JU,*) 'I_ERROR'
 WRITE(JU,*) 'SONDEERLENGTE'
 WRITE(JU,*) 'CONUSWEERSTAND'
 WRITE(JU,*) 'KLEEF'
 WRITE(JU,*) 'WRIJVINGSGETAL'
 WRITE(JU,*) 'WATERSPANNING'
 WRITE(JU,*) '3,TXT'

 IDIR=1
 DO I=1,SIZE(GEFNAMES)
  CALL GEFDEALLOCATE()
  IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Processing '//TRIM(GEFNAMES(I))//' ...')
  GEFNAMES(I)=GEFDIR(:INDEX(GEFDIR,'\',.TRUE.))//GEFNAMES(I)//'.gef'
  IF(LREADGEF(I))THEN

   IATTRIB=0
   DO ICOL=1,NCOL

    IF(INDEX(UTL_CAP(TRIM(ATTRIB2(ICOL)),'U'),'GECORRIGEERDEDIEPTE').GT.0)IATTRIB(1)=ICOL  !<--- preferable
    !## try others
    IF(IATTRIB(1).EQ.0)THEN
     SELECT CASE (UTL_CAP(TRIM(ATTRIB2(ICOL)),'U'))
      CASE ('PENETRATIONLENGTH','SONDEERLENGTE','DIEPTE:','DIEPTE','LENGTE','DEPTH','LENGTH')
       IATTRIB(1)=ICOL
     END SELECT
    ENDIF

    IF(INDEX(UTL_CAP(TRIM(ATTRIB1(ICOL)),'U'),'MPA').GT.0.OR. &
       INDEX(UTL_CAP(TRIM(ATTRIB1(ICOL)),'U'),'KPA').GT.0)THEN
   
     JCOL=0
     SELECT CASE (UTL_CAP(TRIM(ATTRIB2(ICOL)),'U'))
      CASE ('QC','CONUS','CONUSWEERSTAND','CONERESISTANCE','PUNT','PUNTWEERSTAND','PUNTDRUK', &
            'CONUSWEERSTANDQC','TIP','CONUSWAARDE')
       JCOL=2
      CASE ('FS','KLEEF','WRIJVINGSWEERSTAND','WRIJVING','LOKALEWRIJVING','WRIJVINGSWEERSTANDFS')
       JCOL=3
      CASE ('WATERSPANNINGU1','WATERSPANNING','POREPRESSURE','WATERPRESSURE','WATERDRUK','PORIEDRUK', &
            'PORIESPANNING','PIEZOSPANNING','PIEZODRUK','PIEZOPRESSURE','PIEZO','U1','U2')
       JCOL=5
     END SELECT

     IF(JCOL.GT.0)THEN
      IATTRIB(JCOL)=ICOL
          
      FMULT(JCOL)=1.0
      IF(INDEX(UTL_CAP(TRIM(ATTRIB1(ICOL)),'U'),'KPA').GT.0)FMULT(JCOL)=0.001
     ENDIF
     
    ENDIF

    IF(INDEX(UTL_CAP(TRIM(ATTRIB1(ICOL)),'U'),'%').GT.0)THEN
     SELECT CASE (UTL_CAP(TRIM(ATTRIB2(ICOL)),'U'))
      CASE ('WRIJVINGSGETAL','WRIJVINGSGETALRF','RF','WRIJVINGGETAL','LOCALFRICTION', &
            'FRICTIONRATIO','FRICTIONNUMBER')
       IATTRIB(4)=ICOL; FMULT(4)=1.0
     END SELECT
    ENDIF
    
   END DO

   !## write only whenever length is available
   IF(IATTRIB(1).EQ.0)THEN 

    IF(IBATCH.EQ.1)THEN
     DO ICOL=1,NCOL
      WRITE(*,*) ICOL,TRIM(ATTRIB1(ICOL))//','//TRIM(ATTRIB2(ICOL))
     ENDDO
    ENDIF
    
    LINE='"'//TRIM(RTOS(X,'F',2))//'","'//TRIM(RTOS(Y,'F',2))//'","'// &
         TRIM(CID)//'","'//TRIM(RTOS(0.0,'F',2))//'","'//TRIM(ITOS(1))//'"'
    DO ICOL=1,NCOLIPF 
     IF(IATTRIB(ICOL).EQ.0)THEN
      LINE=TRIM(LINE)//',"NotAvailable"'
     ELSE
      LINE=TRIM(LINE)//',"'//TRIM(ATTRIB1(IATTRIB(ICOL)))//'-'//TRIM(ATTRIB2(IATTRIB(ICOL)))//'"'
     ENDIF
    ENDDO
    WRITE(JU,'(A)') TRIM(LINE)

   ELSE

    IF(MOD(I,1000).EQ.0)IDIR=IDIR+1

    CALL IOSDIRMAKE(TRIM(IPFDIR)//'\subset'//TRIM(ITOS(IDIR)))

    ZEND=Z-ABS(GEF(IATTRIB(1),NROW))

    LINE='"'//TRIM(RTOS(X,'F',2))//'","'//TRIM(RTOS(Y,'F',2))//'","subset'//TRIM(ITOS(IDIR))// &
         '\'//TRIM(CID)//'","'//TRIM(RTOS(ZEND,'F',2))//'","'//TRIM(ITOS(0))//'"'
    DO ICOL=1,NCOLIPF 
     IF(IATTRIB(ICOL).EQ.0)THEN
      LINE=TRIM(LINE)//',"NotAvailable"'
     ELSE
      LINE=TRIM(LINE)//',"'//TRIM(ATTRIB1(IATTRIB(ICOL)))//'-'//TRIM(ATTRIB2(IATTRIB(ICOL)))//'"'
     ENDIF
    ENDDO
    WRITE(JU,'(A)') TRIM(LINE)

    !## write associated file
    KU=UTL_GETUNIT()
    OPEN(KU,FILE=TRIM(IPFDIR)//'\subset'//TRIM(ITOS(IDIR))//'\'//TRIM(CID)//'.TXT',STATUS='UNKNOWN',FORM='FORMATTED',ACTION='WRITE')
    LINE=TRIM(ITOS(NROW))
    WRITE(KU,'(A)') TRIM(LINE)
    LINE=TRIM(ITOS(NCOLIPF))//',3'
    WRITE(KU,'(A)') TRIM(LINE)

    DO ICOL=1,NCOLIPF 
     IF(ICOL.EQ.1)LINE='"m,NAP",' 
     IF(ICOL.EQ.2)LINE='"MPa,Conusweerstand",'
     IF(ICOL.EQ.3)LINE='"MPa,Kleef",'
     IF(ICOL.EQ.4)LINE='"%,Wrijvingsgetal",'
     IF(ICOL.EQ.5)LINE='"MPa,Waterspanning",'
     IF(IATTRIB(ICOL).GT.0)THEN
      LINE=TRIM(LINE)//TRIM(RTOS(NODATA(IATTRIB(ICOL)),'*',7))
     ELSE
      LINE=TRIM(LINE)//'-99999'
     ENDIF
     WRITE(KU,*) TRIM(LINE)
    END DO

    DO IROW=1,NROW
     LINE=TRIM(RTOS(Z-ABS(GEF(IATTRIB(1),IROW)),'F',3))
     DO ICOL=2,NCOLIPF 
      IF(IATTRIB(ICOL).NE.0)THEN
       IF(GEF(IATTRIB(ICOL),IROW).NE.NODATA(IATTRIB(ICOL)))THEN
        LINE=TRIM(LINE)//','//TRIM(RTOS(ABS(GEF(IATTRIB(ICOL),IROW))*FMULT(ICOL),'*',7))
       ELSE
        LINE=TRIM(LINE)//','//TRIM(RTOS(GEF(IATTRIB(ICOL),IROW)*FMULT(ICOL),'*',7))
       ENDIF
      ELSE
       LINE=TRIM(LINE)//',-99999.0'
      ENDIF
     END DO
     WRITE(KU,*) TRIM(LINE)
    END DO
    CLOSE(KU)

   ENDIF

   IF(IBATCH.EQ.1)WRITE(6,'(A,F10.2,A)') '+Progress ',REAL(I)/REAL(SIZE(GEFNAMES))*100.0,'%              '  

  ELSE

   LINE='"'//TRIM(RTOS(X,'F',2))//'","'//TRIM(RTOS(Y,'F',2))//'","'// &
         TRIM(CID)//'","'//TRIM(RTOS(0.0,'F',2))//'","'//TRIM(ITOS(1))//'"'
   DO ICOL=1,NCOLIPF; LINE=TRIM(LINE)//',"NotAvailable"'; ENDDO
   WRITE(JU,'(A)') TRIM(LINE)

  ENDIF
 END DO

 CLOSE(JU)
 IF(IBATCH.EQ.0)CALL UTL_MESSAGEHANDLE(1)

 END SUBROUTINE GEF2IPF_MAIN

END MODULE MOD_GEF2IPF
