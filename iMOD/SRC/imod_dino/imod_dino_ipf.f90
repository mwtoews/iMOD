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
MODULE MOD_DINO_IPF

USE RESOURCE
USE MOD_DINO_PAR
USE MOD_UTL, ONLY : ITOS,RTOS,UTL_CREATEDIR,UTL_GETUNIT,UTL_CAP,UTL_CAP_BIG,UTL_INSIDEPOLYGON
USE MOD_OSD, ONLY : OSD_OPEN
USE MODPLOT, ONLY : MPW
USE MOD_POLYGON_PAR
USE MOD_POLYGON_UTL, ONLY : POLYGON1SAVELOADSHAPE

 CONTAINS
 
 !###====================================================================
 SUBROUTINE IMOD_DINO_MAIN()
 !###====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=256) :: FNAME,BIJM
 CHARACTER(LEN=1052) :: LINE
 INTEGER :: ICSV,IU,JU,KU,IOS,IB,I,J,K,II,JB,I1,I2,NY,IND,ISUB,NSUB,IY,TD,IGEOTOP,N
 CHARACTER(LEN=20) :: TXT
 INTEGER :: NUNIQUE,IWINDOW
 CHARACTER(LEN=52),POINTER,DIMENSION(:,:) :: VAR
 
 IWINDOW=1
 IF((MPW%XMIN.EQ.MPW%XMAX).AND.(MPW%YMIN.EQ.MPW%YMAX))IWINDOW=0
 
 CALL IMOD_DINO_DEALLOCATE()
 
 SHPNO=0
 IF(GENFNAME.NE.'')THEN
  CALL POLYGON1SAVELOADSHAPE(ID_LOADSHAPE,0,GENFNAME,VAR)
  SHPIACT(1:SHPNO)=1
  MPW%XMIN=10.0E10; MPW%XMAX=-10.0E10
  MPW%YMIN=10.0E10; MPW%YMAX=-10.0E10
  DO I=1,SHPNO
   MPW%XMIN=MIN(MPW%XMIN,MINVAL(SHPXC(1:SHPNCRD(I),I)))
   MPW%XMAX=MAX(MPW%XMAX,MAXVAL(SHPXC(1:SHPNCRD(I),I)))
   MPW%YMIN=MIN(MPW%YMIN,MINVAL(SHPYC(1:SHPNCRD(I),I)))
   MPW%YMAX=MAX(MPW%YMAX,MAXVAL(SHPYC(1:SHPNCRD(I),I)))
  ENDDO
  IWINDOW=1; N=MAXVAL(SHPNCRD(1:SHPNO))
 ENDIF
 
 ALLOCATE(ND(SIZE(CSVFNAME)))
 
 DO ICSV=1,SIZE(CSVFNAME)
  IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=CSVFNAME(ICSV),STATUS='OLD',ACTION='READ,DENYWRITE')
  IF(IU.EQ.0)THEN; WRITE(*,'(/A/)') 'Cannot open file: '//TRIM(CSVFNAME(ICSV)); RETURN; ENDIF
  WRITE(*,'(1X,A)') 'Reading: '//TRIM(CSVFNAME(ICSV))
  READ(IU,*); ND(ICSV)=0
  DO; READ(IU,*,IOSTAT=IOS); IF(IOS.NE.0)EXIT; ND(ICSV)=ND(ICSV)+1; ENDDO
  CLOSE(IU)
 ENDDO
 
 DO ICSV=1,SIZE(CSVFNAME)
  LINE=' Read '//TRIM(ITOS(ND(ICSV)))//' data points from: '//TRIM(CSVFNAME(ICSV)); WRITE(*,'(A)') TRIM(LINE)
 ENDDO
 
 IF(SUM(ND).LE.0)THEN; WRITE(*,'(/A/)') 'Nothing to import!'; RETURN; ENDIF

 CALL UTL_CREATEDIR(IPFFNAME(:INDEX(IPFFNAME,'\',.TRUE.)-1))
 JU=UTL_GETUNIT(); CALL OSD_OPEN(JU,FILE=TRIM(IPFFNAME)//'.tmp',STATUS='UNKNOWN',ACTION='WRITE')

 WRITE(JU,'(A)') 'NaN:Yet'
 WRITE(JU,'(A)') '5'
 WRITE(JU,'(A)') '"X-coordinate, m"'
 WRITE(JU,'(A)') '"Y-coordinate, m"'
 WRITE(JU,'(A)') '"Identificatie"'
 WRITE(JU,'(A)') '"Maaiveld, m+NAP"'
 WRITE(JU,'(A)') '"Einddiepte, m+NAP"'
 WRITE(JU,'(A)') '3,txt'

 ALLOCATE(LUNIQUE(1)); NUNIQUE=0
 
 NSUB=1; ISUB=0; TD=0
 
 DO ICSV=1,SIZE(CSVFNAME)
  IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=CSVFNAME(ICSV),STATUS='OLD',ACTION='READ,DENYWRITE')
  LINE='Reading: '//TRIM(ITOS(ND(ICSV)))//' records from '//TRIM(CSVFNAME(ICSV))
  WRITE(*,'(1X,A)') TRIM(LINE)
  
  IF(ND(ICSV).LE.0)CYCLE
  
  ALLOCATE(DINO(ND(ICSV)))

  !## number of drills
  IB=1; JB=1

  !## header
  READ(IU,*)
  DO IND=1,ND(ICSV)

   READ(IU,'(A1052)',IOSTAT=IOS) LINE; IF(IOS.NE.0)EXIT

   !## replace all spaces for "_"-characters
   DO; J=INDEX(LINE,' '); IF(J.EQ.0)EXIT; LINE(J:J)='_'; ENDDO
   !## capatlize line
   LINE=UTL_CAP_BIG(LINE,'U')
   
   READ(LINE,*,IOSTAT=IOS) DINO(JB)%OPB,DINO(JB)%DATE,DINO(JB)%ID,DINO(JB)%IX,DINO(JB)%IY
   !## confidential
   IF(TRIM(DINO(JB)%OPB).EQ.'VER')THEN
    READ(DINO(JB)%DATE,'(I4)',IOSTAT=IOS) IY
    IF(IOS.EQ.0)THEN
     !## public before 2009
     IF(IY.LT.2009)DINO(JB)%OPB='OPB'
    ENDIF
   ENDIF
   IF(TRIM(DINO(JB)%OPB).NE.'OPB')CYCLE

   !## in- or outside window
   IF(IWINDOW.EQ.1)THEN
    !## inside given window
     IF(REAL(DINO(JB)%IX).LT.MPW%XMIN.OR. &
        REAL(DINO(JB)%IX).GT.MPW%XMAX.OR. &       
        REAL(DINO(JB)%IY).GT.MPW%YMAX.OR. &       
        REAL(DINO(JB)%IY).LT.MPW%YMIN)DINO(JB)%OPB='OUT'
   ENDIF
   !## inside polygon
   IF(TRIM(DINO(JB)%OPB).NE.'OPB')CYCLE
   
   IF(SHPNO.GT.0)THEN
    DO I=1,SHPNO
     IF(UTL_INSIDEPOLYGON(REAL(DINO(JB)%IX),REAL(DINO(JB)%IY),SHPXC(:,I),SHPYC(:,I),SHPNCRD(I)).EQ.1)EXIT
    ENDDO
    IF(I.GT.SHPNO)DINO(JB)%OPB='OUT'
   ENDIF
   IF(TRIM(DINO(JB)%OPB).NE.'OPB')CYCLE
   
   DINO(JB)%ZMK=''
   DINO(JB)%LITH=''
   DINO(JB)%ZM=''
   DINO(JB)%ZMK=''
   DINO(JB)%AK=''
   DINO(JB)%AS=''
   DINO(JB)%AZ=''
   DINO(JB)%AG=''
   DINO(JB)%AH=''
   DINO(JB)%SH=''
   DINO(JB)%SHFR=''
   DINO(JB)%PLANTFR=''
   DINO(JB)%MICAFR=''
   DINO(JB)%GLAUCFR=''
   DINO(JB)%ORG=''
   DINO(JB)%CACO3=''
   DINO(JB)%CONS=''
   DINO(JB)%COLOR=''
   DINO(JB)%PLANTS=''
   DINO(JB)%SHELLS=''
   
   READ(LINE,*,IOSTAT=IOS) DINO(JB)%OPB,DINO(JB)%DATE,DINO(JB)%ID,DINO(JB)%IX,DINO(JB)%IY,DINO(JB)%MV, &
                           TXT,DINO(JB)%YTOPMV,DINO(JB)%YBOTMV, &
                           DINO(JB)%YTOP,DINO(JB)%YBOT,DINO(JB)%LITH,DINO(JB)%ZM,DINO(JB)%ZMK,     &
                           DINO(JB)%AK,DINO(JB)%AS,DINO(JB)%AZ,DINO(JB)%AG,DINO(JB)%AH,DINO(JB)%SH,  &
                           DINO(JB)%SHFR,DINO(JB)%PLANTFR,DINO(JB)%MICAFR,DINO(JB)%GLAUCFR,DINO(JB)%ORG,DINO(JB)%CACO3, &
                           DINO(JB)%CONS,DINO(JB)%COLOR,DINO(JB)%DESC,DINO(JB)%PLANTS,DINO(JB)%SHELLS

   IF(IOS.EQ.0)THEN 

    IF(JB.GT.1)THEN
     IF(TRIM(DINO(JB-1)%ID).NE.TRIM(DINO(JB)%ID))IB=IB+1
    ENDIF
    
    DO J=1,NUNIQUE
     IF(TRIM(LUNIQUE(J)).EQ.TRIM(DINO(JB)%LITH))EXIT
    END DO
    IF(J.GT.NUNIQUE)THEN
     NUNIQUE=NUNIQUE+1; II=SIZE(LUNIQUE)
     IF(II.LT.NUNIQUE)THEN
      ALLOCATE(TMP(NUNIQUE)); TMP(1:II)=LUNIQUE; DEALLOCATE(LUNIQUE); LUNIQUE=>TMP
     ENDIF
     LUNIQUE(NUNIQUE)=DINO(JB)%LITH
    ENDIF

    JB=JB+1
   ENDIF
  ENDDO

  JB=JB-1

  IF(JB.LE.0)THEN
   WRITE(*,'(/1X,A/)') 'Nothing to import, check file '//TRIM(CSVFNAME(ICSV))
  ELSE

   WRITE(*,'(/A/)') 'Unique values found:'
   DO I=1,NUNIQUE; WRITE(*,*) I,':"'//TRIM(LUNIQUE(I))//'"'; END DO

   WRITE(*,'(/2(A,I10))') ' Read ',ND(ICSV),' observations, remaining ',JB
   WRITE(*,'(A,I10,A/)')  ' Total ',IB,' drills'

   !## number of trajectories within drill
   I1=1; I2=I1; NY=1        

   ND(ICSV)=JB
   DO I=2,ND(ICSV)
    IF(TRIM(DINO(I-1)%ID).EQ.TRIM(DINO(I)%ID).AND.I.LT.ND(ICSV))THEN
     NY=NY+1; I2=I2+1
     !## write results naar txt file
    ELSE
     IF(I.EQ.ND(ICSV))THEN; NY=NY+1; I2=I2+1; ENDIF

     ISUB=ISUB+1
     IF(MOD(ISUB,MAXSUB).EQ.0)THEN; NSUB=NSUB+1; ISUB=0; ENDIF
     J=INDEX(IPFFNAME,'.',.TRUE.)-1; K=INDEX(IPFFNAME,'\',.TRUE.)+1
     FNAME=IPFFNAME(K:J)//'\subset'//TRIM(ITOS(NSUB))
     DINO(I-1)%YEND=DINO(I1)%YBOT 
     !## number of data
     TD=TD+1
     !## write results
     LINE=TRIM(ITOS(DINO(I-1)%IX))//','//TRIM(ITOS(DINO(I-1)%IY))//','//TRIM(FNAME)//'\'//TRIM(DINO(I-1)%ID)//','// &
          TRIM(RTOS(DINO(I-1)%MV/1000.0,'F',2))//','//TRIM(RTOS(DINO(I-1)%YEND/1000.0,'F',2))
     WRITE(JU,'(A)') TRIM(LINE)
     FNAME=IPFFNAME(:J)//'\subset'//TRIM(ITOS(NSUB))
     CALL UTL_CREATEDIR(FNAME)
     FNAME=TRIM(FNAME)//'\'//TRIM(DINO(I-1)%ID)//'.txt'
     KU=UTL_GETUNIT()
     CALL OSD_OPEN(KU,FILE=FNAME,STATUS='UNKNOWN',ACTION='WRITE')
     WRITE(KU,*) (I2-I1)+2 
     WRITE(KU,'(A)') '6,2'
     WRITE(KU,'(A)') '"Grensvlak, m+NAP",-999.99'
     WRITE(KU,'(A)') '"Lithologie",-999.99'
     WRITE(KU,'(A)') '"LithoKlassificatie",-999.99'
     WRITE(KU,'(A)') '"Zandmediaanklasse NEN5104",-999.99'
     WRITE(KU,'(A)') '"Bijmenging",-999.99' 
     WRITE(KU,'(A)') '"Omschrijving",-999.99' 

     DO II=I2,I1,-1
      
      IGEOTOP=IMOD_DINO_LITHOCLASS(TRIM(DINO(II)%LITH),&
                                   TRIM(DINO(II)%ZM),  &
                                   TRIM(DINO(II)%ZMK), &
                                   TRIM(DINO(II)%AZ),  &
                                   TRIM(DINO(II)%AS),  &
                                   TRIM(DINO(II)%AK),  &
                                   TRIM(DINO(II)%SHFR),  &
                                   '0') 
      BIJM=''
      CALL BIJM_TXT(BIJM,DINO(II)%AK)
      CALL BIJM_TXT(BIJM,DINO(II)%AS)
      CALL BIJM_TXT(BIJM,DINO(II)%AZ)
      CALL BIJM_TXT(BIJM,DINO(II)%AG)
      CALL BIJM_TXT(BIJM,DINO(II)%AH)
      IF(BIJM.EQ.'')BIJM='None'
      BIJM=ADJUSTL(BIJM)

      IF(TRIM(DINO(II)%LITH).EQ.'')DINO(II)%LITH='None'
      IF(TRIM(DINO(II)%ZMK).EQ.'') DINO(II)%ZMK='None'
      IF(TRIM(DINO(II)%DESC).EQ.'')DINO(II)%DESC='None'

      LINE=TRIM(RTOS(DINO(II)%YTOP/1000.0,'F',2))//','// &
           '"'//TRIM(DINO(II)%LITH)//'",'// &
           TRIM(ITOS(IGEOTOP))//','// &
           '"'//TRIM(DINO(II)%ZMK)//'",'// &
           '"'//TRIM(BIJM)//'",'// &
           '"'//TRIM(DINO(II)%DESC)//'"'
      WRITE(KU,'(A)') TRIM(LINE)
     END DO
     LINE=TRIM(RTOS(DINO(I1)%YBOT/1000.0,'F',2))//',-,-,-,-,-'
     WRITE(KU,'(A)') TRIM(LINE)
     CLOSE(KU) 
     !## increase/reset counters
     I1=I2+1; I2=I1; NY=0
    ENDIF
   ENDDO
  ENDIF
  CLOSE(IU)
  DEALLOCATE(DINO)
 ENDDO
 CLOSE(JU)
 
 JU=UTL_GETUNIT(); CALL OSD_OPEN(JU,FILE=TRIM(IPFFNAME)//'.tmp',STATUS='OLD',ACTION='READ')
 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=IPFFNAME,STATUS='UNKNOWN',ACTION='WRITE')
 READ(JU,*)
 LINE=TRIM(ITOS(TD))
 WRITE(IU,'(A)') TRIM(LINE)
 DO
  READ(JU,'(A256)',IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT
  WRITE(IU,'(A)') TRIM(LINE)
 ENDDO
 CLOSE(IU); CLOSE(JU,STATUS='DELETE')

 END SUBROUTINE IMOD_DINO_MAIN

 !###====================================================================
 SUBROUTINE BIJM_TXT(BIJM,TXT)
 !###====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(INOUT) :: BIJM
 CHARACTER(LEN=*),INTENT(IN) :: TXT
 CHARACTER(LEN=10),DIMENSION(4) :: C1,C2

 DATA C1/'Siltig','Zandig','Grindig','Humeus'/
 DATA C2/'Zwak','Matig','Sterk','Uiterst'/

 IF(TXT(1:4).EQ.'None')RETURN

 SELECT CASE (TXT(2:2))
  CASE ('X');   BIJM=''
  CASE ('1');   BIJM=TRIM(C2(1))
  CASE ('2');   BIJM=TRIM(C2(2))
  CASE ('3');   BIJM=TRIM(C2(3))
  CASE ('4');   BIJM=TRIM(C2(4))
 END SELECT

 SELECT CASE (TXT(1:1))
  CASE ('S');   BIJM=TRIM(BIJM)//' '//TRIM(C1(1))
  CASE ('Z');   BIJM=TRIM(BIJM)//' '//TRIM(C1(2))
  CASE ('G');   BIJM=TRIM(BIJM)//' '//TRIM(C1(3))
  CASE ('H');   BIJM=TRIM(BIJM)//' '//TRIM(C1(4))
 END SELECT

 END SUBROUTINE

 !###====================================================================
 SUBROUTINE IMOD_DINO_DEALLOCATE()
 !###====================================================================
 IMPLICIT NONE

 IF(ASSOCIATED(LUNIQUE))DEALLOCATE(LUNIQUE)
 IF(ALLOCATED(DINO))DEALLOCATE(DINO)
 IF(ALLOCATED(ND))DEALLOCATE(ND)

 END SUBROUTINE IMOD_DINO_DEALLOCATE

 !###====================================================================
 INTEGER FUNCTION IMOD_DINO_LITHOCLASS(LITH,ZM,ZMK,AZ,ASILT,AK,LUTUM_PCT,KLEIBROKJES)
 !###====================================================================
 !    # parameters:
 !    # lith  = lithologie (code)
 !    # zm    = zandmediaan (getalswaarde)
 !    # zmk   = zandmediaanklasse (code)
 !    # az    = bijmenging zand
 !    # asilt = bijmenging silt
 !    # ak    = bijmenging klei
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: LITH,LUTUM_PCT,ZMK,ASILT,AK,AZ,KLEIBROKJES,ZM
 INTEGER :: ILUTUM_PCT,IZM,IOS
 
 !   # bepaal de lithoklasse / grainsizeklasse op basis van lithologie en zandmediaanklasse

 !   # lithoklasse blijft '-' indien geen lithoklasse bepaald kan worden
 !   # bijvoorbeeld bij lithologie 'NBE' of 'GM'

 !   # 13-07-2009: aanpassingen om verschillen met REGIS weg te nemen (lutum_pct en kleibrokjes)
 !   # 17-10-2009: Lithoklasse 4 (Leem) is komen te vervallen;
 !   #             voor hydrologische toepassingen wordt Leem ondergebracht
 !   #             bij kleiig zand en zandige klei
 !   #             Let op: er is niet hernummerd, met andere woorden,
 !   #                     op lithoklasse 3 volgt lithoklasse 5.
 !   # 26-04-2011: JSch: Lithologie GCZ (glauconietzand) toegevoegd en behandeld cf. Lithologie Z. 

 IMOD_DINO_LITHOCLASS=0
  
 !   # lutum_pct numeriek maken
 READ(LUTUM_PCT,*,IOSTAT=IOS) ILUTUM_PCT
 IF(IOS.NE.0)ILUTUM_PCT=9999
!     if lutum_pct in ['None','']:
!        lutum_pct = '9999'
!    lutum_pct = int(lutum_pct)
 
 SELECT CASE (TRIM(LITH))

  CASE ('V','GY','DY','DET','HO'); IMOD_DINO_LITHOCLASS=1 !## organic fuber
!    if lith in ['V','GY','DY','DET','HO']:
!        lithoklasse = 1 # organische stof

  CASE ('K')
   SELECT CASE (AZ)
    CASE ('ZX','Z1','Z2','Z3'); IMOD_DINO_LITHOCLASS=3   !## kleiig zand en zandige klei 
!    elif lith in ['K'] and az in ['ZX','Z1','Z2','Z3']:
!        lithoklasse = 3 # kleiig zand en zandige klei 
   END SELECT
   IF(IMOD_DINO_LITHOCLASS.EQ.0)THEN
    SELECT CASE (ASILT)
     CASE ('SX','S3','S4'); IMOD_DINO_LITHOCLASS=3        !## kleiig zand en zandige klei
!    elif lith in ['K'] and asilt in ['SX','S3','S4']:
!        lithoklasse = 3 # kleiig zand en zandige klei 
    END SELECT
   ENDIF
   IF(IMOD_DINO_LITHOCLASS.EQ.0)THEN
    IF(ILUTUM_PCT.NE.9999.AND.ILUTUM_PCT.LT.35)IMOD_DINO_LITHOCLASS=3 !## kleiig zand en zandige klei
!    elif lith in ['K'] and lutum_pct != 9999 and lutum_pct < 35:
!        lithoklasse = 3 # kleiig zand en zandige klei 
    IF(IMOD_DINO_LITHOCLASS.EQ.0)IMOD_DINO_LITHOCLASS=2 !## klei 
!    elif lith in ['K']:
!        lithoklasse = 2 # klei
   ENDIF

  CASE ('L')
   IMOD_DINO_LITHOCLASS=3 !# kleiig zand en zandige klei                       
!    elif lith in ['L']:
!        lithoklasse = 3 # kleiig zand en zandige klei                       
 
  CASE ('Z','GCZ')
   SELECT CASE (TRIM(ZMK))
    CASE ('None','','ZMO'); IMOD_DINO_LITHOCLASS=10 !## zand overig
!    elif lith in ['Z','GCZ'] and zmk in ['None','','ZMO']:
!        lithoklasse = 10 # zand overig
    CASE ('ZFC','ZUF','ZUFO','ZZF','ZZFO')
     IF((TRIM(AK).EQ.'K3'.OR.TRIM(AK).EQ.'KX').OR. &
         TRIM(KLEIBROKJES).EQ.'1'.OR. &
        (ILUTUM_PCT.GE.5.AND.ILUTUM_PCT.NE.9999))IMOD_DINO_LITHOCLASS=3 !## kleiig zand en zandige klei     
!    elif (lith in ['Z','GCZ'] and zmk in ['ZFC','ZUF','ZUFO','ZZF','ZZFO'] and
!         (ak in ['K3','KX']
!             or kleibrokjes in ['1']
!             or (lutum_pct >= 5 and lutum_pct != 9999))):
!        lithoklasse = 3 # kleiig zand en zandige klei
   END SELECT
   IF(IMOD_DINO_LITHOCLASS.EQ.0)THEN
    SELECT CASE (TRIM(ZMK))
     CASE ('ZFC','ZUF','ZUFO','ZZF','ZZFO'); IMOD_DINO_LITHOCLASS=5 !## fijn zand cf. REGIS              
!    elif lith in ['Z','GCZ'] and zmk in ['ZFC','ZUF','ZUFO','ZZF','ZZFO']: 
!        # 63 - 150
!        lithoklasse = 5 # fijn zand cf. REGIS              
    END SELECT
   ENDIF
   IF(IMOD_DINO_LITHOCLASS.EQ.0)THEN
    SELECT CASE (TRIM(ZMK))
     CASE ('ZMC','ZMF','ZMFO','ZMG','ZMGO'); IMOD_DINO_LITHOCLASS=6 !## matig grof cf. REGIS                    
!    elif lith in ['Z','GCZ'] and zmk in ['ZMC','ZMF','ZMFO','ZMG','ZMGO']: 
!        # 150 - 300
!        lithoklasse = 6 # matig grof cf. REGIS                    
    END SELECT
   ENDIF       
   IF(IMOD_DINO_LITHOCLASS.EQ.0)THEN
    SELECT CASE (TRIM(ZMK))
     CASE ('ZGC','ZZG','ZZGO','ZUG','ZUGO'); IMOD_DINO_LITHOCLASS=7 !## grof cf. REGIS                  
!    elif lith in ['Z','GCZ'] and zmk in ['ZGC','ZZG','ZZGO','ZUG','ZUGO']: 
!        # 300 - 2000
!        lithoklasse = 7 # grof cf. REGIS                  
    END SELECT
   ENDIF

  CASE ('G','STN'); IMOD_DINO_LITHOCLASS=8 !## grind
!    elif lith in ['G','STN']: 
!        # > 2000
!        lithoklasse = 8 # grind

  CASE ('SHE'); IMOD_DINO_LITHOCLASS=9 !## schelpen
!    elif lith in ['SHE']: 
!        # > 2000
!        lithoklasse = 9 # schelpen

 END SELECT

!    # bepaal de grainsize op basis van zandmediaan
!    # deze overschrijft de eerdere grainsize classificatie
 SELECT CASE (TRIM(LITH))
  CASE ('Z','GCZ')
   IF(TRIM(ZM).NE.'NONE')THEN
!    if lith in ['Z','GCZ'] and zm not in ['None','']:
    READ(ZM,*,IOSTAT=IOS) IZM
!    WRITE(*,*) zm,ios,trim(lith)
    IF(IOS.EQ.0)THEN !STOP 'ZM=IZM'
     IF(IZM.GE.63.AND.IZM.LT.150.AND. &
        ((TRIM(AK).EQ.'K3'.OR.TRIM(AK).EQ.'KX').OR. &
          TRIM(KLEIBROKJES).EQ.'1'.OR. &
         (ILUTUM_PCT.GE.5.AND.ILUTUM_PCT.NE.9999)))THEN
!        if (zm >= 63 and zm < 150 and
!           (ak in ['K3','KX']
!               or kleibrokjes in ['1']
!               or (lutum_pct >= 5 and lutum_pct != 9999))):
!            lithoklasse = 3 # kleiig zand en zandige klei
      IMOD_DINO_LITHOCLASS=3     !## kleiig zand en zandige klei
     ELSEIF(IZM.GE.63.AND.IZM.LT.150)THEN
!        elif zm >= 63 and zm < 150:
!            lithoklasse = 5
      IMOD_DINO_LITHOCLASS=5
     ELSEIF(IZM.GE.150.AND.IZM.LT.300)THEN
!        elif zm >= 150 and zm < 300:
!            lithoklasse = 6                    
      IMOD_DINO_LITHOCLASS=6
     ELSEIF(IZM.GE.300.AND.IZM.LT.2000)THEN
!        elif zm >= 300 and zm < 2000:
!            lithoklasse = 7             
      IMOD_DINO_LITHOCLASS=7
     ELSEIF(IZM.GE.2000)THEN
!        elif zm >= 2000:
!            lithoklasse = 8               
      IMOD_DINO_LITHOCLASS=8
     ENDIF
    ENDIF
   ENDIF
 END SELECT

 END FUNCTION IMOD_DINO_LITHOCLASS

END MODULE MOD_DINO_IPF