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
MODULE MOD_GEF2IPF

USE WINTERACTER
USE RESOURCE
USE MOD_GEF2IPF_PAR
USE MOD_UTL, ONLY : UTL_CREATEDIR,UTL_MESSAGEHANDLE

CONTAINS

 !###======================================================================
 SUBROUTINE GEF2IPF_MAIN(IBATCH,GEFTYPE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH,GEFTYPE
 INTEGER :: ITYPE,IGEFTYPE
 TYPE(WIN_MESSAGE) :: MESSAGE
 
 IGEFTYPE=GEFTYPE
 
 IF(GEFTYPE.EQ.0)THEN
  CALL WDIALOGLOAD(ID_DGEF)
  CALL WDIALOGPUTRADIOBUTTON(IDF_RADIO1)
  CALL WDIALOGSHOW(-1,-1,0,2)
 
  DO
   CALL WMESSAGE(ITYPE,MESSAGE)
   SELECT CASE(ITYPE) 
    CASE (PUSHBUTTON)
     SELECT CASE (MESSAGE%VALUE1)
      CASE (IDCANCEL)
       EXIT
      CASE (IDOK)
       CALL WDIALOGGETRADIOBUTTON(IDF_RADIO1,IGEFTYPE)
       EXIT
      CASE (IDHELP)
     END SELECT
   END SELECT
  ENDDO
 
  CALL WDIALOGUNLOAD() 
 
 ENDIF 
 
 IF(IGEFTYPE.EQ.1)CALL GEF2IPF_GEFCPT(IBATCH)
 IF(IGEFTYPE.EQ.2)CALL GEF2IPF_GEFBORE(IBATCH)
 
 END SUBROUTINE GEF2IPF_MAIN
 
 !###======================================================================
 SUBROUTINE GEF2IPF_GEFCPT(IBATCH)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH
 INTEGER :: I,ICOL,IROW,KU,IDIR,JCOL
 CHARACTER(LEN=256) :: LINE,IPFDIR
 INTEGER,PARAMETER :: NCOLIPF=5
 
 WRITE(*,*) "Reading GEF-file with probing information"
 
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
  IF(IBATCH.EQ.1)WRITE(*,'(A)') 'Processing '//TRIM(GEFNAMES(I))//' ...'
  GEFNAMES(I)=GEFDIR(:INDEX(GEFDIR,'\',.TRUE.))//GEFNAMES(I)//'.gef'
  IF(LREADGEF_CPT(I))THEN

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
    LINE=TRIM(IPFDIR)//'\subset'//TRIM(ITOS(IDIR))//'\'//TRIM(CID)//'.TXT'
    OPEN(KU,FILE=TRIM(LINE),STATUS='UNKNOWN',FORM='FORMATTED',ACTION='WRITE')
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

 END SUBROUTINE GEF2IPF_GEFCPT

 !###======================================================================
 SUBROUTINE GEF2IPF_GEFBORE(IBATCH)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH
 INTEGER :: I,J,ICOL,IROW,KU,IDIR,JCOL
 CHARACTER(LEN=256) :: IPFDIR
 CHARACTER(LEN=512) :: LINE
 INTEGER,PARAMETER :: NCOLIPF=22
 
 WRITE(*,*) "Reading GEF-file with boring hole information"
 
 IF(.NOT.ASSOCIATED(GEFNAMES))THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not find any files!','Error'); RETURN
 ENDIF

 IF(IBATCH.EQ.0)CALL UTL_MESSAGEHANDLE(0)

 I=INDEX(IPFFNAME,'\',.TRUE.)-1; IPFDIR=IPFFNAME(:I); CALL UTL_CREATEDIR(IPFDIR)
 JU=UTL_GETUNIT(); OPEN(JU,FILE=TRIM(IPFDIR)//IPFFNAME(I+1:),STATUS='UNKNOWN',ACTION='WRITE',FORM='FORMATTED')
 WRITE(JU,*) SIZE(GEFNAMES)
 WRITE(JU,*) NCOLIPF
 WRITE(JU,*) 'X'
 WRITE(JU,*) 'Y'
 WRITE(JU,*) 'ID'
 WRITE(JU,*) 'Z_END'
 WRITE(JU,*) 'I_ERROR'
 WRITE(JU,*) 'DIEPTEBOVENKANTLAAG'
 WRITE(JU,*) 'DIEPTEONDERKANTLAAG'
 WRITE(JU,*) 'ZANDMEDIAAN'
 WRITE(JU,*) 'GRINDMEDIAAN'
 WRITE(JU,*) 'LUTUMPERCENTAGE'
 WRITE(JU,*) 'SILTPERCENTAGE'
 WRITE(JU,*) 'ZANDPERCENTAGE'
 WRITE(JU,*) 'GRINDPERCENTAGE'
 WRITE(JU,*) 'ORGANISCHESTOFPERCENTAGE'
 WRITE(JU,*) 'LITHOLOGY'
 DO I=1,(NCOLIPF-15)
  WRITE(JU,*) 'EXTRAVARIABLE'
 ENDDO
 WRITE(JU,*) '3,TXT'

 IDIR=1
 DO I=1,SIZE(GEFNAMES)
  CALL GEFDEALLOCATE()
  IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Processing '//TRIM(GEFNAMES(I))//' ...')
  IF(IBATCH.EQ.1)WRITE(*,'(A)') 'Processing '//TRIM(GEFNAMES(I))//' ...'
  GEFNAMES(I)=GEFDIR(:INDEX(GEFDIR,'\',.TRUE.))//GEFNAMES(I)//'.gef'
  IF(LREADGEF_BORE(I))THEN

   IATTRIB=0
   DO ICOL=1,MAXVAL(NCOLLINE)
    IF(INDEX(UTL_CAP(TRIM(ATTRIB2(ICOL)),'U'),'DIEPTEBOVENKANTLAAG').GT.0)IATTRIB(1)=ICOL
    IF(INDEX(UTL_CAP(TRIM(ATTRIB2(ICOL)),'U'),'DIEPTEONDERKANTLAAG').GT.0)IATTRIB(2)=ICOL
    IF(INDEX(UTL_CAP(TRIM(ATTRIB2(ICOL)),'U'),'ZANDMEDIAAN').GT.0)IATTRIB(3)=ICOL
    IF(INDEX(UTL_CAP(TRIM(ATTRIB2(ICOL)),'U'),'GRINDMEDIAAN').GT.0)IATTRIB(4)=ICOL
    IF(INDEX(UTL_CAP(TRIM(ATTRIB2(ICOL)),'U'),'LITHOLOGY').GT.0)IATTRIB(10)=ICOL
    
    IF(ICOL.GE.NCOL+2)THEN
     DO J=11,MAXVAL(NCOLLINE)
      IF(INDEX(UTL_CAP(TRIM(ATTRIB2(ICOL)),'U'),'EXTRA VARIABLE').GT.0)IATTRIB(J)=ICOL
     ENDDO
    ENDIF
    
     JCOL=0
     IF(INDEX(UTL_CAP(TRIM(ATTRIB1(ICOL)),'U'),'%').GT.0)THEN
      SELECT CASE (UTL_CAP(TRIM(ATTRIB2(ICOL)),'U'))
       CASE ('LUTUMPERCENTAGE')
        JCOL=5
       CASE ('SILTPERCENTAGE')
        JCOL=6
       CASE ('ZANDPERCENTAGE')
        JCOL=7
       CASE ('GRINDPERCENTAGE')
        JCOL=8
       CASE ('ORGANISCHESTOFPERCENTAGE')
        JCOL=9
      END SELECT
      
      IF(JCOL.GT.0)THEN
       IATTRIB(JCOL)=ICOL
      ENDIF
     
     ENDIF
    
   END DO
      
   !## write only whenever length is available
   IF(IATTRIB(1).EQ.0)THEN 

    IF(IBATCH.EQ.1)THEN
     DO ICOL=1,MAXVAL(NCOLLINE)
      WRITE(*,*) ICOL,TRIM(ATTRIB1(ICOL))//','//TRIM(ATTRIB2(ICOL))
     ENDDO
    ENDIF
    
    LINE='"'//TRIM(RTOS(X,'F',2))//'","'//TRIM(RTOS(Y,'F',2))//'","'// &
         TRIM(CID)//'","'//TRIM(RTOS(0.0,'F',2))//'","'//TRIM(ITOS(1))//'"'
    DO ICOL=1,(MAXVAL(NCOLLINE)+5)   
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

    ZEND=Z-ABS(GEF(IATTRIB(2),NROW))
    
    LINE='"'//TRIM(RTOS(X,'F',2))//'","'//TRIM(RTOS(Y,'F',2))//'","subset'//TRIM(ITOS(IDIR))// &
         '\'//TRIM(CID)//'","'//TRIM(RTOS(ZEND,'F',2))//'","'//TRIM(ITOS(0))//'"'
    DO ICOL=1,(MAXVAL(NCOLLINE)+5)
     IF(IATTRIB(ICOL).EQ.0)THEN
      LINE=TRIM(LINE)//',"NotAvailable"'
     ELSE
      LINE=TRIM(LINE)//',"'//TRIM(ATTRIB1(IATTRIB(ICOL)))//'-'//TRIM(ATTRIB2(IATTRIB(ICOL)))//'"'
     ENDIF
    ENDDO
    WRITE(JU,'(A)') TRIM(LINE)

    !## write associated file
    KU=UTL_GETUNIT()
    LINE=TRIM(IPFDIR)//'\subset'//TRIM(ITOS(IDIR))//'\'//TRIM(CID)//'.TXT'
    OPEN(KU,FILE=TRIM(LINE),STATUS='UNKNOWN',FORM='FORMATTED',ACTION='WRITE')
    LINE=TRIM(ITOS(NROW+1))
    WRITE(KU,'(A)') TRIM(LINE)
    LINE=TRIM(ITOS(MAXVAL(NCOLLINE)))//',2'
    WRITE(KU,'(A)') TRIM(LINE)

    DO ICOL=1,NCOL+1
     IF(ICOL.EQ.1)LINE='"m,NAP",' 
     IF(ICOL.EQ.2)LINE='"m,NAP",'
     IF(ICOL.EQ.3)LINE='"mm,Zandmediaan",'
     IF(ICOL.EQ.4)LINE='"mm,Grindmediaan",'
     IF(ICOL.EQ.5)LINE='"%,Lutum percentage",'
     IF(ICOL.EQ.6)LINE='"%,Silt percentage",'
     IF(ICOL.EQ.7)LINE='"%,Zand percentage",'
     IF(ICOL.EQ.8)LINE='"%,Grind percentage",'
     IF(ICOL.EQ.9)LINE='"%,Organisch stof percentage",'
     IF(ICOL.EQ.10)LINE='"[],Lithology",'
     
     IF(IATTRIB(ICOL).GT.0)THEN
      LINE=TRIM(LINE)//TRIM(RTOS(NODATA(IATTRIB(ICOL)),'*',1))
     ELSE
      LINE=TRIM(LINE)//'-99999'
     ENDIF
     WRITE(KU,*) TRIM(LINE)
    END DO
    
    DO ICOL=11,MAXVAL(NCOLLINE)
     LINE='"[],Extra variable",-9999.990'
     WRITE(KU,*) TRIM(LINE)
    ENDDO
    
    DO IROW=1,NROW
     LINE=TRIM(RTOS(Z-ABS(GEF(IATTRIB(1),IROW)),'F',1))
     LINE=TRIM(LINE)//','//TRIM(RTOS(Z-ABS(GEF(IATTRIB(2),IROW)),'F',1))
     DO ICOL=3,NCOL 
      IF(IATTRIB(ICOL).NE.0)THEN
       IF(GEF(IATTRIB(ICOL),IROW).NE.NODATA(IATTRIB(ICOL)))THEN
        LINE=TRIM(LINE)//','//TRIM(RTOS(ABS(GEF(IATTRIB(ICOL),IROW)),'*',1))
       ELSE
        LINE=TRIM(LINE)//','//TRIM(RTOS(GEF(IATTRIB(ICOL),IROW),'*',1))
       ENDIF
      ELSE
       LINE=TRIM(LINE)//',-99999.0'
      ENDIF
     END DO
     DO ICOL=1,NCOLLINE(IROW)-(NCOL+1)
      IF(GEFEX(ICOL,IROW).NE.'')THEN
       LINE=TRIM(LINE)//','//TRIM(GEFEX(ICOL,IROW))
      ELSE
       LINE=TRIM(LINE)//','''
      ENDIF
     ENDDO
     WRITE(KU,*) TRIM(LINE)
    END DO
    
    LINE=TRIM(RTOS(Z-ABS(GEF(IATTRIB(2),NROW)),'F',1))
    DO ICOL=2,NCOLIPF
     LINE=TRIM(LINE)//',-'
    ENDDO
    WRITE(KU,*) TRIM(LINE)
        
    CLOSE(KU)
    
   ENDIF

   IF(IBATCH.EQ.1)WRITE(6,'(A,F10.2,A)') '+Progress ',REAL(I)/REAL(SIZE(GEFNAMES))*100.0,'%              '  

  ELSE

   LINE='"'//TRIM(RTOS(X,'F',2))//'","'//TRIM(RTOS(Y,'F',2))//'","'// &
         TRIM(CID)//'","'//TRIM(RTOS(0.0,'F',2))//'","'//TRIM(ITOS(1))//'"'
   DO ICOL=1,NCOLLINE(IROW); LINE=TRIM(LINE)//',"NotAvailable"'; ENDDO
   WRITE(JU,'(A)') TRIM(LINE)

  ENDIF
 END DO

 CLOSE(JU)
 IF(IBATCH.EQ.0)CALL UTL_MESSAGEHANDLE(1)

 END SUBROUTINE GEF2IPF_GEFBORE

END MODULE MOD_GEF2IPF
