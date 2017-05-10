MODULE MOD_DEVWEL

USE WINTERACTER
USE RESOURCE
USE IMOD, ONLY : IDFINIT
USE MOD_UTL, ONLY : UTL_CREATEDIR,UTL_GENLABELSREAD,NV,NL,VAR,UTL_GENLABELSDEALLOCATE,UTL_GETUNIT, &
   MAXLEN,ITOS,UTL_CAP,UTL_ROTATE_XYZ,PI,RTOS,UTL_DATA_CSV,ICOL_VAR,IACT_VAR,CCNST,UTL_WSELECTFILE
USE MOD_PMANAGER, ONLY : PMANAGER_SAVEMF2005_MAXNO

CONTAINS

 !###======================================================================
 SUBROUTINE DEVWELL_IMPORT(CSVFNAME,IPFFNAME,ICOLS,IBATCH)
 !###======================================================================
 IMPLICIT NONE
 REAL,PARAMETER :: G2R=360.0/(2.0*PI)
 INTEGER,INTENT(IN) :: IBATCH
 CHARACTER(LEN=*),INTENT(IN) :: CSVFNAME,IPFFNAME
 INTEGER,DIMENSION(:),POINTER,INTENT(INOUT) :: ICOLS
 CHARACTER(LEN=256) :: DIR,LINE,FNAME
 CHARACTER(LEN=6) :: CT
 REAL :: X,Y,AX,AY,AZ,X1,Y1,Z1,DEM,L,DX,DY,DZ,TL1,TL2
 INTEGER :: I,II,J,I1,I2,N,IU,JU,NLC,IOS
 CHARACTER(LEN=MAXLEN) :: CL1,CL2,CL

 IF(IBATCH.EQ.0)THEN     !123456789012                  12345678901234567890
  IF(.NOT.UTL_DATA_CSV((/'Name        ','X Coordinate','Y Coordinate', &
                         'Z Coordinate','Depth       ','Inclination ', &
                         'Azimuth     ','Add. Label 1','Add. Label 2', &
                         'Add. Label 3','Add. Label 4'/),VAR,ICOL_VAR,IACT_VAR,CCNST))RETURN

  IF(.NOT.UTL_WSELECTFILE('Save IPF File (*.ipf)|*.ipf|',&
                   SAVEDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,FNAME,&
                   'Save IPF File (*.ipf)'))THEN
   CALL UTL_GENLABELSDEALLOCATE(); RETURN
  ENDIF
  
  J=0; DO I=1,SIZE(IACT_VAR); IF(IACT_VAR(I).EQ.1)J=J+1; ENDDO
  ALLOCATE(ICOLS(J))
  J=0; DO I=1,SIZE(IACT_VAR)
   IF(IACT_VAR(I).EQ.1)THEN; J=J+1; ICOLS(J)=ICOL_VAR(I); ENDIF
  ENDDO
  NLC=0; DO I=8,SIZE(IACT_VAR); IF(IACT_VAR(I).EQ.1)NLC=NLC+1; ENDDO
 
 ELSE
 
  NLC=SIZE(ICOLS)-7
 
  FNAME=IPFFNAME
  
  WRITE(*,'(A)') 'Reading '//TRIM(CSVFNAME)//' ...'
  CALL UTL_GENLABELSREAD(CSVFNAME,VAR,NL,NV)

  WRITE(*,'(/A)') 'Read info:'
  WRITE(*,'(A,I10)') 'Number of records',NL
  WRITE(*,'(A,I10)') 'Number of columns',NV
 
  DO I=1,NV
   CT=''
   DO J=1,SIZE(ICOLS)
    IF(ICOLS(J).EQ.I)THEN
     SELECT CASE (J)
      CASE (1);  CT='NAME'
      CASE (2);  CT='XCRD'
      CASE (3);  CT='YCRD'
      CASE (4);  CT='ZCRD'
      CASE (5);  CT='DEPT'
      CASE (6);  CT='INCL'
      CASE (7);  CT='AZIM'
      CASE (8:); CT='LABL'//TRIM(ITOS(J-7))
     END SELECT
    ENDIF
   ENDDO
   IF(LEN_TRIM(CT).EQ.0)THEN
    WRITE(*,'(6X,A9,2X,A)') 'COLUMN'//TRIM(ITOS(I)),VAR(I,0)
   ELSE
    WRITE(*,'(A6,A9,2X,A)') CT,' COLUMN'//TRIM(ITOS(I)),VAR(I,0)
   ENDIF  
  ENDDO

 ENDIF

 DIR=FNAME(:INDEX(FNAME,'\',.TRUE.)-1)
 CALL UTL_CREATEDIR(DIR)

 !## process data 
 IU=UTL_GETUNIT(); OPEN(IU,FILE=TRIM(FNAME)//'_',STATUS='UNKNOWN',ACTION='WRITE')
 WRITE(IU,'(A)') 'NaN1#'
 WRITE(IU,'(A)') '5'
 WRITE(IU,'(A)') 'XCRD'
 WRITE(IU,'(A)') 'YCRD'
 WRITE(IU,'(A)') 'ZCRD'
 WRITE(IU,'(A)') 'NAME'
 WRITE(IU,'(A)') 'ID'
 WRITE(IU,'(A)') '5,TXT'
 
 N=0; I1=0; DO
  I1=I1+1
  
  CL1=UTL_CAP(VAR(ICOLS(1),I1),'U')  
  !## determine entire borehole
  I2=I1; DO
   CL2=UTL_CAP(VAR(ICOLS(1),I2),'U')  
   IF(TRIM(CL1).NE.TRIM(CL2))EXIT
   I2=I2+1; IF(I2.GT.NL)EXIT
  ENDDO
  I2=I2-1
  
  !## clean cl for forbidden characters
  DO; II=INDEX(TRIM(CL1),'/'); IF(II.EQ.0)EXIT; CL1(II:II)='_'; ENDDO
  DO; II=INDEX(TRIM(CL1),':'); IF(II.EQ.0)EXIT; CL1(II:II)='_'; ENDDO
  DO; II=INDEX(TRIM(CL1),' '); IF(II.EQ.0)EXIT; CL1(II:II)='_'; ENDDO
    
  JU=UTL_GETUNIT(); OPEN(JU,FILE=TRIM(DIR)//'\'//TRIM(CL1)//'.TXT',STATUS='UNKNOWN',ACTION='WRITE')
  WRITE(JU,'(I4.4)') I2-I1+1
  WRITE(JU,'(I3,A)') 3+NLC,',4'
  WRITE(JU,'(A)') 'DX,-999.99'
  WRITE(JU,'(A)') 'DY,-999.99'
  WRITE(JU,'(A)') 'Z ,-999.99'
  DO I=1,NLC
   WRITE(JU,'(A,F7.2)') TRIM(VAR(ICOLS(7+I),0))//',',-999.99
  ENDDO
  !## process current borehole
  X1=0.0; Y1=0.0; TL1=0.0
  !## read z value
  READ(VAR(ICOLS(4),I1),*,IOSTAT=IOS) Z1
  IF(IOS.NE.0)THEN; WRITE(*,'(/1X,A/)') 'Error reading elevation from '//TRIM(VAR(ICOLS(4),I1)); STOP; ENDIF
  DO II=I1,I2

   !## read total length through well
   READ(VAR(ICOLS(5),II),*) TL2

   !## no rotation for x-axes
   AX=0.0
   !## read inclination - rotation for y-axes
   READ(VAR(ICOLS(6),II),*) AY
   !## read azimuth - rotation for z-axes
   READ(VAR(ICOLS(7),II),*) AZ
   
   !## convert to radians
   AX=AX/G2R; AY=AY/G2R; AZ=AZ/G2R
   AY=AY-0.5*PI
   
   !## length
   L=TL2-TL1
   
   !## get point in depth
   X =L*COS(AY)
   Y =0.0
   DZ=L*SIN(AY)

   !## rotate point for azimuth 
   DX= COS(AZ)*X+SIN(AZ)*Y
   DY=-SIN(AZ)*X+COS(AZ)*Y

   X1=X1+DX
   Y1=Y1+DY
   Z1=Z1+DZ
   
   !## add labels
   LINE=TRIM(RTOS(X1,'F',2))//','//TRIM(RTOS(Y1,'F',2))//','//TRIM(RTOS(Z1,'F',2))
   DO I=1,NLC
    !## add dummy label if missing
    IF(ICOLS(7+I).LE.0)THEN
     CL='S'
    ELSE
     READ(VAR(ICOLS(7+I),II),*) CL
    ENDIF
    LINE=TRIM(LINE)//',"'//TRIM(CL)//'"'
   ENDDO
   WRITE(JU,'(A)') TRIM(LINE)
   
   TL1=TL2
   
  ENDDO        
  CLOSE(JU)

  !## increase number of wells
  N=N+1

  !## write information to ipf: x,y,name,id
  WRITE(IU,'(A)') TRIM(VAR(ICOLS(2),I1))//',' // &
                  TRIM(VAR(ICOLS(3),I1))//',' // &
                  TRIM(VAR(ICOLS(4),I1))//',"'// &
                  TRIM(VAR(ICOLS(1),I1))//'",'//TRIM(CL1)

  !## continue with rest
  I1=I2
  !## stop - finished
  IF(I1.EQ.NL)EXIT  
 ENDDO

 CLOSE(IU)
 
 CALL PMANAGER_SAVEMF2005_MAXNO(TRIM(FNAME)//'_',(/N/))
  
 CALL UTL_GENLABELSDEALLOCATE(); DEALLOCATE(ICOLS)
 
 !## load ipf into imod manager
 IF(IBATCH.EQ.0)CALL IDFINIT(FNAME,LPLOT=.TRUE.) 
 
 END SUBROUTINE DEVWELL_IMPORT
 
END MODULE MOD_DEVWEL