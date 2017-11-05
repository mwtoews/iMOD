MODULE MOD_DEVWEL

USE WINTERACTER
USE RESOURCE
USE IMOD, ONLY : IDFINIT
USE MOD_UTL, ONLY : UTL_CREATEDIR,UTL_GENLABELSREAD,NV,NL,VAR,UTL_GENLABELSDEALLOCATE,UTL_GETUNIT, &
   MAXLEN,ITOS,UTL_CAP,UTL_ROTATE_XYZ,PI,RTOS,UTL_DATA_CSV,ICOL_VAR,IACT_VAR,CCNST,UTL_WSELECTFILE
USE MOD_PMANAGER, ONLY : PMANAGER_SAVEMF2005_MAXNO
USE MOD_GENPLOT, ONLY : TOPOGENINIT

CONTAINS

 !###======================================================================
 SUBROUTINE DEVFAULT_IMPORT
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IU,JU,IOS,I,J,M,NP
 CHARACTER(LEN=256) :: LINE,FNAME
 DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE :: X,Y,Z
  
! !##v41
! CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'Oops, this functionality is under construction and not yet available in this release.','Information')
! RETURN

 IF(.NOT.UTL_WSELECTFILE('Load ASC File (*.asc)|*.asc|',&
                  LOADDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,FNAME,&
                  'Load ASC File (*.asc)'))RETURN

 !## process data 
 IU=UTL_GETUNIT(); OPEN(IU,FILE=FNAME,STATUS='OLD'    ,ACTION='READ' )
 FNAME=FNAME(:INDEX(FNAME,'.',.TRUE.))//'GEN'
 JU=UTL_GETUNIT(); OPEN(JU,FILE=FNAME,STATUS='UNKNOWN',ACTION='WRITE')

 DO I=1,7 ; READ(IU,*); ENDDO
 NP=0; DO
  READ(IU,'(10X,A1)') LINE
  IF(UTL_CAP(LINE(1:1),'U').NE.'X')EXIT
  NP=NP+1
 ENDDO
 DO I=1,8 ; READ(IU,*); ENDDO

 ALLOCATE(X(NP,2),Y(NP,2),Z(NP,2))
 
 READ(IU,'(A5)',IOSTAT=IOS) LINE

 !## start processing the lines
 M=0; DO
  IF(IOS.NE.0)EXIT

  !## start new fault
  IF(TRIM(UTL_CAP(LINE(1:5),'U')).EQ.'FAULT')THEN
   READ(IU,'(A256)') LINE
   J=0; DO
    J=J+1
    READ(LINE,'(10X,3(F15.0,1X))') X(1,2),Y(1,2),Z(1,2)
    DO I=2,NP
     READ(IU,'(10X,3(F15.0,1X))') X(I,2),Y(I,2),Z(I,2)
    ENDDO
    DO I=1,3; READ(IU,*); ENDDO

    READ(IU,'(A256)',IOSTAT=IOS) LINE

    !## write GEN-file
    IF(J.GT.1)THEN
     DO I=1,NP-1
      M=M+1
      WRITE(JU,*) M
      WRITE(JU,'(3(F15.7,1X))') X(I  ,1),Y(I  ,1),Z(I  ,1)
      WRITE(JU,'(3(F15.7,1X))') X(I+1,1),Y(I+1,1),Z(I+1,1)
      WRITE(JU,'(3(F15.7,1X))') X(I+1,2),Y(I+1,2),Z(I+1,2)
      WRITE(JU,'(3(F15.7,1X))') X(I  ,2),Y(I  ,2),Z(I  ,2)
      WRITE(JU,'(3(F15.7,1X))') X(I  ,1),Y(I  ,1),Z(I  ,1)
      WRITE(JU,'(A)') 'END'
     ENDDO
    ENDIF

    IF(IOS.NE.0)EXIT
    !## stop reading pillars
    IF(TRIM(UTL_CAP(LINE(1:6),'U')).NE.'PILLAR')EXIT

    DO I=1,NP; X(I,1)=X(I,2); Y(I,1)=Y(I,2); Z(I,1)=Z(I,2); ENDDO

   ENDDO
   
  ENDIF
 
 ENDDO 
 WRITE(JU,'(A)') 'END'
 
!FAULT     "Fault 1" 0    
!PILLAR    445205.72190970 6151162.82328465 -1910.15419048
!          445205.72190970 6151162.82328465 -1568.19109181
!          445205.72190970 6151162.82328465 -1226.22799315
!          445205.72190970 6151162.82328465 -884.26489449
!          445205.72190970 6151162.82328465 -542.30179583
!          0.00000000 0.00000000
!          0     UNDEF UNDEF UNDEF 
!          Linear A-DIR UNDEF SEGMENT FALSE
!PILLAR    445610.19806679 6151456.78699928 -1905.14043167
!          445610.19806679 6151456.78699928 -1563.54074399
!          445610.19806679 6151456.78699928 -1221.94105630
!          445610.19806679 6151456.78699928 -880.34136862
!          445610.19806679 6151456.78699928 -538.74168093
!          0.00000000 0.00000000
!          0     UNDEF UNDEF UNDEF 
!          Linear A-DIR UNDEF SEGMENT FALSE

 CLOSE(IU); CLOSE(JU)
 DEALLOCATE(X,Y,Z)

 CALL TOPOGENINIT(GENNAME=FNAME,LPLOT=.TRUE.,GENCOLOUR=WRGB(255,0,0))

 END SUBROUTINE DEVFAULT_IMPORT

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
 REAL :: X,Y,AX,AY,AZ,X1,Y1,Z1,L,DX,DY,DZ,TL1,TL2
 INTEGER :: I,II,J,I1,I2,N,IU,JU,NLC,IOS
 CHARACTER(LEN=MAXLEN) :: CL1,CL2,CL

! !##v41
! CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'Oops, this functionality is under construction and not yet available in this release.','Information')
! RETURN

 IF(IBATCH.EQ.0)THEN     
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
 
  DO J=1,SIZE(ICOLS)
   CT=''
   DO I=1,NV
    IF(ICOLS(J).EQ.I)THEN
     SELECT CASE (J)
      CASE (1);  CT='NAME'; EXIT
      CASE (2);  CT='XCRD'; EXIT
      CASE (3);  CT='YCRD'; EXIT
      CASE (4);  CT='ZCRD'; EXIT
      CASE (5);  CT='DEPT'; EXIT
      CASE (6);  CT='INCL'; EXIT
      CASE (7);  CT='AZIM'; EXIT
      CASE (8:); CT='LABL'//TRIM(ITOS(J-7)); EXIT
     END SELECT
    ENDIF
   ENDDO
   IF(LEN_TRIM(CT).NE.0)THEN
!    WRITE(*,'(6X,A9,2X,A)') 'COLUMN'//TRIM(ITOS(I)),'' !VAR(I,0)
!   ELSE
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
  WRITE(JU,'(I3,A)') 3+MAX(1,NLC),',4'
  WRITE(JU,'(A)') 'DX,-999.99'
  WRITE(JU,'(A)') 'DY,-999.99'
  WRITE(JU,'(A)') 'Z ,-999.99'
  IF(NLC.EQ.0)THEN
   WRITE(JU,'(A,F7.2)') 'LITHOLOGY,',-999.99
  ELSE
   DO I=1,NLC
    WRITE(JU,'(A,F7.2)') TRIM(VAR(ICOLS(7+I),0))//',',-999.99
   ENDDO
  ENDIF
  !## process current borehole
  X1=0.0; Y1=0.0; TL1=0.0
  IF(ICOLS(4).LE.0)THEN
   Z1=0.0
  ELSE
   !## read z value
   READ(VAR(ICOLS(4),I1),*,IOSTAT=IOS) Z1
   IF(IOS.NE.0)THEN; WRITE(*,'(/1X,A/)') 'Error reading elevation from '//TRIM(VAR(ICOLS(4),I1)); STOP; ENDIF
  ENDIF
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

   !## correction to be sure north is 0.0 and east is 90 degrees   
   AZ=AZ-0.5*PI

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
   IF(NLC.GT.0)THEN
    DO I=1,NLC
     !## add dummy label if missing
     IF(ICOLS(7+I).LE.0)THEN
      CL='S'
     ELSE
      READ(VAR(ICOLS(7+I),II),*) CL
     ENDIF
     LINE=TRIM(LINE)//',"'//TRIM(CL)//'"'
    ENDDO
   ELSE
    LINE=TRIM(LINE)//',S'
   ENDIF
   WRITE(JU,'(A)') TRIM(LINE)
   
   TL1=TL2
   
  ENDDO        
  CLOSE(JU)

  !## increase number of wells
  N=N+1

  !## write information to ipf: x,y,name,id
  IF(ICOLS(4).LE.0)THEN
   WRITE(IU,'(A)') TRIM(VAR(ICOLS(2),I1))//',' // &
                   TRIM(VAR(ICOLS(3),I1))//',' // &
                   TRIM(RTOS(0.0,'F',1))//',"'// &
                   TRIM(VAR(ICOLS(1),I1))//'",'//TRIM(CL1)
  ELSE
   WRITE(IU,'(A)') TRIM(VAR(ICOLS(2),I1))//',' // &
                   TRIM(VAR(ICOLS(3),I1))//',' // &
                   TRIM(VAR(ICOLS(4),I1))//',"'// &
                   TRIM(VAR(ICOLS(1),I1))//'",'//TRIM(CL1)
  ENDIF
  
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