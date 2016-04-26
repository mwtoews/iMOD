c   Copyright (C) Stichting Deltares, 2005-2014.
c
c   This file is part of iMOD.
c
c   This program is free software: you can redistribute it and/or modify
c   it under the terms of the GNU General Public License as published by
c   the Free Software Foundation, either version 3 of the License, or
c   (at your option) any later version.
c
c   This program is distributed in the hope that it will be useful,
c   but WITHOUT ANY WARRANTY; without even the implied warranty of
c   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c   GNU General Public License for more details.
c
c   You should have received a copy of the GNU General Public License
c   along with this program.  If not, see <http://www.gnu.org/licenses/>.
c
c   Contact: imod.support@deltares.nl
c   Stichting Deltares
c   P.O. Box 177
c   2600 MH Delft, The Netherlands.
c
c   iMod is partly based on the USGS MODFLOW2005 source code;
c   for iMOD the USGS MODFLOW2005 source code has been expanded
c   and extensively modified by Stichting Deltares.
c   The original USGS MODFLOW2005 source code can be downloaded from the USGS
c   website http://www.usgs.gov/. The original MODFLOW2005 code incorporated
c   in this file is covered by the USGS Software User Rights Notice;
c   you should have received a copy of this notice along with this program.
c   If not, see <http://water.usgs.gov/software/help/notice/>.

      MODULE GWFLPFMODULE
        INTEGER, SAVE,   POINTER ::ILPFCB,IWDFLG,IWETIT,IHDWET
        INTEGER, SAVE,   POINTER ::ISFAC,ICONCV,ITHFLG,NOCVCO,NOVFC
        INTEGER, SAVE,   POINTER ::IMINKD,IMINC                         ! DLT
        REAL,    SAVE,   POINTER ::MINKD,MINC                           ! DLT
        REAL,    SAVE,   POINTER ::WETFCT
        INTEGER, SAVE,   POINTER, DIMENSION(:)     ::LAYTYP
        INTEGER, SAVE,   POINTER, DIMENSION(:)     ::LAYAVG
        REAL,    SAVE,   POINTER, DIMENSION(:)     ::CHANI
        INTEGER, SAVE,   POINTER, DIMENSION(:)     ::LAYVKA
        INTEGER, SAVE,   POINTER, DIMENSION(:)     ::LAYWET
        INTEGER, SAVE,   POINTER, DIMENSION(:)     ::LAYSTRT
        INTEGER, SAVE,   POINTER, DIMENSION(:,:)   ::LAYFLG
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::VKA
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::VKCB
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::SC1
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::SC2
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::HANI
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::WETDRY
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::HK
      TYPE GWFLPFTYPE
        INTEGER, POINTER ::ILPFCB,IWDFLG,IWETIT,IHDWET
        INTEGER, POINTER ::ISFAC,ICONCV,ITHFLG,NOCVCO,NOVFC
        INTEGER, POINTER ::IMINKD,IMINC                                 ! DLT
        REAL,    POINTER ::MINKD,MINC                                   ! DLT
        REAL, POINTER    ::WETFCT
        INTEGER,   POINTER, DIMENSION(:)     ::LAYTYP
        INTEGER,   POINTER, DIMENSION(:)     ::LAYAVG
        REAL,      POINTER, DIMENSION(:)     ::CHANI
        INTEGER,   POINTER, DIMENSION(:)     ::LAYVKA
        INTEGER,   POINTER, DIMENSION(:)     ::LAYWET
        INTEGER,   POINTER, DIMENSION(:)     ::LAYSTRT
        INTEGER,   POINTER, DIMENSION(:,:)   ::LAYFLG
        REAL,      POINTER, DIMENSION(:,:,:) ::VKA
        REAL,      POINTER, DIMENSION(:,:,:) ::VKCB
        REAL,      POINTER, DIMENSION(:,:,:) ::SC1
        REAL,      POINTER, DIMENSION(:,:,:) ::SC2
        REAL,      POINTER, DIMENSION(:,:,:) ::HANI
        REAL,      POINTER, DIMENSION(:,:,:) ::WETDRY
        REAL,      POINTER, DIMENSION(:,:,:) ::HK
      END TYPE
      TYPE(GWFLPFTYPE) GWFLPFDAT(10)
      END MODULE GWFLPFMODULE


      SUBROUTINE GWF2LPF7AR(IN,IGRID)
C     ******************************************************************
C     ALLOCATE AND READ DATA FOR LAYER PROPERTY FLOW PACKAGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,ITRSS,LAYHDT,LAYHDS,LAYCBD,
     1                      NCNFBD,IBOUND,BUFF,BOTM,NBOTM,DELR,DELC,IOUT
     2                      ,cc,cv,iunit,                               ! ANIPWT
     3                      lbotm,                                      ! DLT
     4                      kdsv                                        ! ANIPWT
      USE GWFBASMODULE,ONLY:HDRY
      USE GWFLPFMODULE,ONLY:ILPFCB,IWDFLG,IWETIT,IHDWET,
     1                      ISFAC,ICONCV,ITHFLG,NOCVCO,NOVFC,WETFCT,
     2                      LAYTYP,LAYAVG,CHANI,LAYVKA,LAYWET,LAYSTRT,
     3                      LAYFLG,VKA,VKCB,SC1,SC2,HANI,WETDRY,HK,
     4                      IMINKD,IMINC,MINKD,MINC                     ! DLT
      use m_mf2005_iu, only: iuani,iupwt
C
      CHARACTER*14 LAYPRN(5),AVGNAM(3),TYPNAM(2),VKANAM(2),WETNAM(2),
     1            HANNAM
      DATA AVGNAM/'      HARMONIC','   LOGARITHMIC','     LOG-ARITH'/
      DATA TYPNAM/'      CONFINED','   CONVERTIBLE'/
      DATA VKANAM/'    VERTICAL K','    ANISOTROPY'/
      DATA WETNAM/'  NON-WETTABLE','      WETTABLE'/
      DATA HANNAM/'      VARIABLE'/
      CHARACTER*200 LINE
      CHARACTER*24 ANAME(9),STOTXT
      CHARACTER*4 PTYP
C
      DATA ANAME(1) /'   HYD. COND. ALONG ROWS'/
      DATA ANAME(2) /'  HORIZ. ANI. (COL./ROW)'/
      DATA ANAME(3) /'     VERTICAL HYD. COND.'/
      DATA ANAME(4) /' HORIZ. TO VERTICAL ANI.'/
      DATA ANAME(5) /'QUASI3D VERT. HYD. COND.'/
      DATA ANAME(6) /'        SPECIFIC STORAGE'/
      DATA ANAME(7) /'          SPECIFIC YIELD'/
      DATA ANAME(8) /'        WETDRY PARAMETER'/
      DATA ANAME(9) /'     STORAGE COEFFICIENT'/

      real :: t, b, d, kd                                               ! DLT

C     ------------------------------------------------------------------
C1------Allocate scalar data.
      ALLOCATE(ILPFCB,IWDFLG,IWETIT,IHDWET)
      ALLOCATE(IMINKD,IMINC,MINKD,MINC)                                 ! DLT
      ALLOCATE(ISFAC,ICONCV,ITHFLG,NOCVCO,NOVFC)
      ALLOCATE(WETFCT)
      ZERO=0.
C
C2------IDENTIFY PACKAGE
      WRITE(IOUT,1) IN
    1 FORMAT(1X,/1X,'LPF -- LAYER-PROPERTY FLOW PACKAGE, VERSION 7',
     1', 5/2/2005',/,9X,'INPUT READ FROM UNIT ',I4)
C
C3------READ COMMENTS AND ITEM 1.
      CALL URDCOM(IN,IOUT,LINE)
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ILPFCB,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,HDRY,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPLPF,R,IOUT,IN)
C
C3A-----WRITE ITEM 1
      IF(ILPFCB.LT.0) WRITE(IOUT,8)
    8 FORMAT(1X,'CONSTANT-HEAD CELL-BY-CELL FLOWS WILL BE PRINTED',
     1  ' WHEN ICBCFL IS NOT 0')
      IF(ILPFCB.GT.0) WRITE(IOUT,9) ILPFCB
    9 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT ',I4)
      WRITE(IOUT,11) HDRY
   11 FORMAT(1X,'HEAD AT CELLS THAT CONVERT TO DRY=',1PG13.5)
      IF(NPLPF.GT.0) THEN
         WRITE(IOUT,15) NPLPF
   15    FORMAT(1X,I5,' Named Parameters     ')
      ELSE
         NPLPF=0
         WRITE(IOUT,'(A)') ' No named parameters'
      END IF
C
C3B-----GET OPTIONS.
      ISFAC=0
      ICONCV=0
      ITHFLG=0
      NOCVCO=0
      NOVFC=0
      IMINKD=0                                                          ! DLT
      IMINC=0                                                           ! DLT
      MINKD=0.                                                          ! DLT
      MINC=0.                                                           ! DLT
      STOTXT=ANAME(6)
   20 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'STORAGECOEFFICIENT') THEN
         ISFAC=1
         STOTXT=ANAME(9)
         WRITE(IOUT,21)
   21    FORMAT(1X,'STORAGECOEFFICIENT OPTION:',/,
     1     1X,'Read storage coefficient rather than specific storage')
      ELSE IF(LINE(ISTART:ISTOP).EQ.'CONSTANTCV') THEN
         ICONCV=1
         WRITE(IOUT,23)
   23    FORMAT(1X,'CONSTANTCV OPTION:',/,1X,'Constant vertical',
     1         ' conductance for convertible layers')
      ELSE IF(LINE(ISTART:ISTOP).EQ.'THICKSTRT') THEN
         ITHFLG=1
         WRITE(IOUT,25)
   25    FORMAT(1X,'THICKSTRT OPTION:',/,1X,'Negative LAYTYP indicates',
     1 ' confined layer with thickness computed from STRT-BOT')
      ELSE IF(LINE(ISTART:ISTOP).EQ.'NOCVCORRECTION') THEN
         NOCVCO=1
         WRITE(IOUT,27)
   27    FORMAT(1X,'NOCVCORRECTION OPTION:',/,1X,
     1    'Do not adjust vertical conductance when applying',
     2              ' the vertical flow correction')
      ELSE IF(LINE(ISTART:ISTOP).EQ.'NOVFC') THEN
         NOVFC=1
         NOCVCO=1
         WRITE(IOUT,29)
   29    FORMAT(1X,'NOVFC OPTION:',/,1X,
     1    'Do not apply the vertical flow correction')
      ELSE IF(LINE(ISTART:ISTOP).EQ.'MINKD') THEN                       ! DLT
         IMINKD=1                                                       ! DLT
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)              ! DLT
         READ(LINE(ISTART:ISTOP),*) MINKD                               ! DLT
      ELSE IF(LINE(ISTART:ISTOP).EQ.'MINC') THEN                        ! DLT
         IMINC=1                                                        ! DLT
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)              ! DLT
         READ(LINE(ISTART:ISTOP),*) MINC                                ! DLT
      ENDIF
      IF(LLOC.LT.200) GO TO 20
C
C4------ALLOCATE AND READ LAYTYP, LAYAVG, CHANI, LAYVKA, LAYWET, LAYSTRT.
      ALLOCATE(LAYTYP(NLAY))
      ALLOCATE(LAYAVG(NLAY))
      ALLOCATE(CHANI(NLAY))
      ALLOCATE(LAYVKA(NLAY))
      ALLOCATE(LAYWET(NLAY))
      ALLOCATE(LAYSTRT(NLAY))
      READ(IN,*) (LAYTYP(K),K=1,NLAY)
      READ(IN,*) (LAYAVG(K),K=1,NLAY)
      READ(IN,*) (CHANI(K),K=1,NLAY)
      READ(IN,*) (LAYVKA(K),K=1,NLAY)
      READ(IN,*) (LAYWET(K),K=1,NLAY)
C
C4A-----PRINT A TABLE OF VALUES FOR LAYTYP, LAYAVG, CHANI, LAYVKA, LAYWET.
      WRITE(IOUT,47)
   47 FORMAT(1X,/3X,'LAYER FLAGS:',/1X,
     1 'LAYER       LAYTYP        LAYAVG         CHANI ',
     2 '       LAYVKA        LAYWET',/1X,75('-'))
      DO 50 K=1,NLAY
      WRITE(IOUT,48) K,LAYTYP(K),LAYAVG(K),CHANI(K),LAYVKA(K),LAYWET(K)
   48 FORMAT(1X,I4,2I14,1PE14.3,2I14)
C
C4A1----SET GLOBAL HEAD-DEPENDENT TRANSMISSIVITY AND STORAGE FLAGS.
      IF (LAYTYP(K).NE.0) THEN
        LAYHDT(K)=1
        LAYHDS(K)=1
      ELSE
        LAYHDT(K)=0
        LAYHDS(K)=0
      ENDIF
   50 CONTINUE
C
C4A2----SET LAYSTRT AND RESET LAYTYP IF THICKSTRT OPTION IS ACTIVE.
      DO 60 K=1,NLAY
      LAYSTRT(K)=0
      IF(LAYTYP(K).LT.0 .AND. ITHFLG.NE.0) THEN
         LAYSTRT(K)=1
         LAYTYP(K)=0
         LAYHDT(K)=0
         LAYHDS(K)=0
         WRITE(IOUT,57) K
   57    FORMAT(1X,'Layer',I5,
     1  ' is confined because LAYTYP<0 and THICKSTRT option is active')
      END IF
   60 CONTINUE
C
C4B-----BASED ON LAYTYP, LAYAVG, CHANI, LAYWET, COUNT THE NUMBER OF EACH
C4B-----TYPE OF 2-D ARRAY; CHECK VALUES FOR CONSISTENCY; AND SETUP
C4B-----POINTERS IN LAYTYP, CHANI, AND LAYWET FOR CONVENIENT ACCESS
C4B-----TO SC2, HANI, and WETDRY.  PRINT INTERPRETED VALUES OF FLAGS.
      NCNVRT=0
      NHANI=0
      NWETD=0
      WRITE(IOUT,67)
   67 FORMAT(1X,/3X,'INTERPRETATION OF LAYER FLAGS:',/1X,
     1  '                       INTERBLOCK     HORIZONTAL',
     2  '    DATA IN',/1X,
     3  '        LAYER TYPE   TRANSMISSIVITY   ANISOTROPY',
     4  '   ARRAY VKA   WETTABILITY',/1X,
     5  'LAYER      (LAYTYP)      (LAYAVG)       (CHANI)',
     6  '      (LAYVKA)      (LAYWET)',/1X,75('-'))
      DO 100 K=1,NLAY
      IF(LAYTYP(K).NE.0) THEN
         NCNVRT=NCNVRT+1
         LAYTYP(K)=NCNVRT
      END IF
      IF(CHANI(K).LE.ZERO) THEN
         NHANI=NHANI+1
         CHANI(K)=-NHANI
      END IF
      IF(LAYWET(K).NE.0) THEN
         IF(LAYTYP(K).EQ.0) THEN
            WRITE(IOUT,*)
     1          ' LAYWET is not 0 and LAYTYP is 0 for layer:',K
            WRITE(IOUT,*) ' LAYWET must be 0 if LAYTYP is 0'
            CALL USTOP(' ')
         ELSE
            NWETD=NWETD+1
            LAYWET(K)=NWETD
         END IF
      END IF
      IF(LAYAVG(K).LT.0 .OR. LAYAVG(K).GT.2) THEN
         WRITE(IOUT,74) LAYAVG(K)
   74    FORMAT(1X,I8,
     1    ' IS AN INVALID LAYAVG VALUE -- MUST BE 0, 1, or 2')
         CALL USTOP(' ')
      END IF
      LAYPRN(1)=TYPNAM(1)
      IF(LAYTYP(K).NE.0) LAYPRN(1)=TYPNAM(2)
      LAYPRN(2)=AVGNAM(LAYAVG(K)+1)
      IF(CHANI(K).LE.0) THEN
         LAYPRN(3)=HANNAM
      ELSE
         WRITE(LAYPRN(3),'(1PE14.3)') CHANI(K)
      END IF
      LAYPRN(4)=VKANAM(1)
      IF(LAYVKA(K).NE.0) LAYPRN(4)=VKANAM(2)
      LAYPRN(5)=WETNAM(1)
      IF(LAYWET(K).NE.0) LAYPRN(5)=WETNAM(2)
      WRITE(IOUT,78) K,(LAYPRN(I),I=1,5)
   78 FORMAT(1X,I4,5A)
  100 CONTINUE
C
C4C-----PRINT WETTING INFORMATION.
      IF(NWETD.EQ.0) THEN
         WRITE(IOUT,13)
   13    FORMAT(1X,/,1X,'WETTING CAPABILITY IS NOT ACTIVE IN ANY LAYER')
         IWDFLG=0
      ELSE
         WRITE(IOUT,12) NWETD
   12    FORMAT(1X,/,1X,'WETTING CAPABILITY IS ACTIVE IN',I4,' LAYERS')
         IWDFLG=1
         READ(IN,*) WETFCT,IWETIT,IHDWET
         IF(IWETIT.LE.0) IWETIT=1
         WRITE(IOUT,*) ' WETTING FACTOR=',WETFCT
         WRITE(IOUT,*) ' WETTING ITERATION INTERVAL=',IWETIT
         WRITE(IOUT,*) ' IHDWET=',IHDWET
      END IF
C
C5------ALLOCATE MEMORY FOR ARRAYS.
      ALLOCATE(LAYFLG(6,NLAY))
      ALLOCATE(HK(NCOL,NROW,NLAY))
      ALLOCATE(VKA(NCOL,NROW,NLAY))
      IF(NCNFBD.GT.0) THEN
         ALLOCATE(VKCB(NCOL,NROW,NCNFBD))
      ELSE
         ALLOCATE(VKCB(1,1,1))
      END IF
      IF(ITRSS.NE.0) THEN
         ALLOCATE(SC1(NCOL,NROW,NLAY))
      ELSE
         ALLOCATE(SC1(1,1,1))
      END IF
      IF(ITRSS.NE.0 .AND. NCNVRT.GT.0) THEN
         ALLOCATE(SC2(NCOL,NROW,NCNVRT))
      ELSE
         ALLOCATE(SC2(1,1,1))
      END IF
      IF(NHANI.GT.0) THEN
         ALLOCATE(HANI(NCOL,NROW,NHANI))
      ELSE
         ALLOCATE(HANI(1,1,1))
      END IF
      IF(NWETD.GT.0) THEN
         ALLOCATE(WETDRY(NCOL,NROW,NWETD))
      ELSE
         ALLOCATE(WETDRY(1,1,1))
      END IF
C
C6------READ PARAMETER DEFINITIONS
      NPHK=0
      NPVKCB=0
      NPVK=0
      NPVANI=0
      NPSS=0
      NPSY=0
      NPHANI=0
      IF(NPLPF.GT.0) THEN
         WRITE(IOUT,115)
  115    FORMAT(/,' PARAMETERS DEFINED IN THE LPF PACKAGE')
         DO 120 K=1,NPLPF
         CALL UPARARRRP(IN,IOUT,N,1,PTYP,1,0,-1)
C   Note that NPHK and the other NP variables in
C   this group are used only as flags, not counts
         IF(PTYP.EQ.'HK') THEN
            NPHK=1
         ELSE IF(PTYP.EQ.'HANI') THEN
C6A-----WHEN A HANI PARAMETER IS USED, THEN ALL HORIZONTAL ANISOTROPY
C6A-----MUST BE DEFINED USING PARAMETERS.  ENSURE THAT ALL CHANI <= 0
            DO 118 I = 1, NLAY
              IF (CHANI(I).GT.0.0) THEN
                WRITE(IOUT,117)
  117           FORMAT(/,
     &' ERROR: WHEN A HANI PARAMETER IS USED, CHANI FOR ALL LAYERS',/,
     &' MUST BE LESS THAN OR EQUAL TO 0.0 -- STOP EXECUTION',
     &' (GWF2LPF7AR)')
                CALL USTOP(' ')
              ENDIF
  118       CONTINUE
            NPHANI=1
         ELSE IF(PTYP.EQ.'VKCB') THEN
            NPVKCB=1
         ELSE IF(PTYP.EQ.'VK') THEN
            NPVK=1
            CALL SGWF2LPF7CK(IOUT,N,'VK  ')
         ELSE IF(PTYP.EQ.'VANI') THEN
            NPVANI=1
            CALL SGWF2LPF7CK(IOUT,N,'VANI')
         ELSE IF(PTYP.EQ.'SS') THEN
            NPSS=1
         ELSE IF(PTYP.EQ.'SY') THEN
            NPSY=1
         ELSE
            WRITE(IOUT,*) ' Invalid parameter type for LPF Package'
            CALL USTOP(' ')
         END IF
  120    CONTINUE
      END IF
C
C7------DEFINE DATA FOR EACH LAYER -- VIA READING OR NAMED PARAMETERS.
      DO 200 K=1,NLAY
      KK=K
C
C7A-----DEFINE HORIZONTAL HYDRAULIC CONDUCTIVITY (HK)
      IF(NPHK.EQ.0) THEN
         CALL U2DREL(HK(:,:,KK),ANAME(1),NROW,NCOL,KK,IN,IOUT)
      ELSE
         READ(IN,*) LAYFLG(1,K)
         WRITE(IOUT,121) ANAME(1),K,LAYFLG(1,K)
  121    FORMAT(1X,/1X,A,' FOR LAYER',I4,
     1   ' WILL BE DEFINED BY PARAMETERS',/1X,'(PRINT FLAG=',I4,')')
         CALL UPARARRSUB1(HK(:,:,KK),NCOL,NROW,KK,'HK',
     1      IOUT,ANAME(1),LAYFLG(1,KK))
         CALL UPARARRCK(BUFF,IBOUND,IOUT,K,NCOL,NLAY,NROW,'HK  ')
      END IF
C
C7B-----READ HORIZONTAL ANISOTROPY IF CHANI IS NON-ZERO
      IF(CHANI(K).LE.ZERO) THEN
        KHANI=-CHANI(K)
        IF(NPHANI.EQ.0) THEN
           CALL U2DREL(HANI(:,:,KHANI),ANAME(2),NROW,NCOL,KK,IN,IOUT)
        ELSE
           READ(IN,*) LAYFLG(6,K)
           WRITE(IOUT,121) ANAME(2),K,LAYFLG(6,K)
           CALL UPARARRSUB1(HANI(:,:,KHANI),NCOL,NROW,KK,'HANI',
     &      IOUT,ANAME(2),LAYFLG(6,KK))
           CALL UPARARRCK(BUFF,IBOUND,IOUT,K,NCOL,NLAY,NROW,'HANI')
        END IF
      END IF
C
C7C-----DEFINE VERTICAL HYDRAULIC CONDUCTIVITY OR HORIZONTAL TO VERTICAL
C7C-----ANISOTROPY (VKA).
      IANAME=3
      PTYP='VK'
      IF(LAYVKA(K).NE.0) THEN
         IANAME=4
         PTYP='VANI'
      END IF
      IF(NPVK.EQ.0 .AND. NPVANI.EQ.0) THEN
         CALL U2DREL(VKA(:,:,KK),ANAME(IANAME),NROW,NCOL,KK,IN,IOUT)
      ELSE
         READ(IN,*) LAYFLG(2,K)
         WRITE(IOUT,121) ANAME(IANAME),K,LAYFLG(2,K)
         CALL UPARARRSUB1(VKA(:,:,KK),NCOL,NROW,KK,PTYP,IOUT,
     &                       ANAME(IANAME),LAYFLG(2,KK))
         CALL UPARARRCK(BUFF,IBOUND,IOUT,K,NCOL,NLAY,NROW,PTYP)
      END IF
C
C7D-----DEFINE SPECIFIC STORAGE OR STORAGE COEFFICIENT IN ARRAY SC1 IF TRANSIENT.
      IF(ITRSS.NE.0) THEN
         IF(NPSS.EQ.0) THEN
            CALL U2DREL(SC1(:,:,KK),STOTXT,NROW,NCOL,KK,IN,IOUT)
         ELSE
            READ(IN,*) LAYFLG(3,K)
            WRITE(IOUT,121) STOTXT,K,LAYFLG(3,K)
            CALL UPARARRSUB1(SC1(:,:,KK),NCOL,NROW,KK,'SS',
     1           IOUT,STOTXT,LAYFLG(3,KK))
            CALL UPARARRCK(BUFF,IBOUND,IOUT,K,NCOL,NLAY,NROW,'SS  ')
         END IF
         IF(ISFAC.EQ.0) THEN
            CALL SGWF2LPF7SC(SC1(:,:,KK),KK,1)
         ELSE
            CALL SGWF2LPF7SC(SC1(:,:,KK),KK,0)
         END IF
      END IF
C
C7E-----DEFINE SPECIFIC YIELD IN ARRAY SC2 IF TRANSIENT AND LAYER IS
C7E-----IS CONVERTIBLE.
      IF(LAYTYP(K).NE.0) THEN
         IF(ITRSS.NE.0) THEN
            IF(NPSY.EQ.0) THEN
               CALL U2DREL(SC2(:,:,LAYTYP(K)),ANAME(7),NROW,NCOL,KK,IN,
     1                 IOUT)
            ELSE
               READ(IN,*) LAYFLG(4,K)
               WRITE(IOUT,121) ANAME(7),K,LAYFLG(4,K)
               CALL UPARARRSUB1(SC2(:,:,LAYTYP(K)),NCOL,
     1         NROW,KK,'SY',IOUT,ANAME(7),LAYFLG(4,KK))
               CALL UPARARRCK(BUFF,IBOUND,IOUT,K,NCOL,NLAY,NROW,'SY  ')
            END IF
            CALL SGWF2LPF7SC(SC2(:,:,LAYTYP(K)),KK,0)
         END IF
      END IF
C
C7F-----READ CONFINING BED VERTICAL HYDRAULIC CONDUCTIVITY (VKCB)
      IF(LAYCBD(K).NE.0) THEN
         IF(NPVKCB.EQ.0) THEN
            CALL U2DREL(VKCB(:,:,LAYCBD(K)),ANAME(5),NROW,NCOL,KK,IN,
     1             IOUT)
         ELSE
            READ(IN,*) LAYFLG(5,K)
            WRITE(IOUT,121) ANAME(5),K,LAYFLG(5,K)
            CALL UPARARRSUB1(VKCB(:,:,LAYCBD(K)),NCOL,NROW,KK,
     1         'VKCB',IOUT,ANAME(5),LAYFLG(5,KK))
            CALL UPARARRCK(BUFF,IBOUND,IOUT,K,NCOL,NLAY,NROW,'VKCB')
         END IF
      END IF
C
C7G-----READ WETDRY CODES IF WETTING CAPABILITY HAS BEEN INVOKED
C7G-----(LAYWET NOT 0).
      IF(LAYWET(K).NE.0) THEN
         CALL U2DREL(WETDRY(:,:,LAYWET(K)),ANAME(8),NROW,NCOL,KK,IN,
     1            IOUT)
      END IF
  200 CONTINUE
C
      call pest1alpha_grid('KH',hk,nrow,ncol,nlay)                      ! IPEST
      call pest1alpha_grid('KV',vka,nrow,ncol,nlay)                     ! IPEST
      call pest1alpha_grid('VA',vkcb,nrow,ncol,nlay)                    ! IPEST
      if(itrss.ne.0) call pest1alpha_grid('SC',sc1,nrow,ncol,nlay)      ! IPEST

      if (iminkd.eq.1) then                                             ! DLT
         do ilay=1,nlay                                                 ! DLT
            do irow=1,nrow                                              ! DLT
               do icol=1,ncol                                           ! DLT
                  t = botm(icol,irow,lbotm(ilay)-1)                     ! DLT
                  b = botm(icol,irow,lbotm(ilay))                       ! DLT
                  !## minimal thickness is zero                         ! DLT
                  d=max(0.0,t-b)                                        ! DLT
                  !## adjust kh such that minkd will be satisfied       ! DLT
                  kd=max(minkd,d*hk(icol,irow,ilay))                    ! DLT
                  if(d.gt.0.0)hk(icol,irow,ilay)=kd/d                   ! DLT
               end do                                                   ! DLT
            end do                                                      ! DLT
         end do                                                         ! DLT
      end if                                                            ! DLT
C
      if (IUNIT(IUANI).gt.0.or.IUNIT(IUPWT).gt.0) then                  ! ANIPWT
         do ilay=1,nlay                                                 ! ANIPWT
            do irow=1,nrow                                              ! ANIPWT
               do icol=1,ncol                                           ! ANIPWT
                  t = botm(icol,irow,lbotm(ilay)-1)                     ! ANIPWT
                  b = botm(icol,irow,lbotm(ilay))                       ! ANIPWT
                  !## minimal thickness is zero                         ! ANIPWT
                  d=max(0.0,t-b)                                        ! ANIPWT
                  !prevent transmissivity to be zero                    ! ANIPWT
                  kdsv(icol,irow,ilay) = hk(icol,irow,ilay)*d           ! ANIPWT
                  if (iminkd.eq.1) then                                 ! ANIPWT
                      kdsv(icol,irow,ilay)=                             ! ANIPWT
     1                   max(minkd,kdsv(icol,irow,ilay))                ! ANIPWT
                  end if                                                ! ANIPWT
                  kdsv(icol,irow,ilay)=max(1.0e-12,kdsv(icol,irow,ilay))! ANIPWT
               end do                                                   ! ANIPWT
            end do                                                      ! ANIPWT
         end do                                                         ! ANIPWT
      end if                                                            ! ANIPWT
C
C8------PREPARE AND CHECK LPF DATA.
      CALL SGWF2LPF7N()
C
      if (IUNIT(IUANI).gt.0.or.IUNIT(IUPWT).gt.0) then                  ! DLT
         do ilay=1,nlay-1                                               ! DLT
            do irow=1,nrow                                              ! DLT
               do icol=1,ncol                                           ! DLT
                  !prevent vcont to be zero                             ! DLT
                  cv(icol,irow,ilay)=max(1.0e-12,cv(icol,irow,ilay))    ! DLT
               end do                                                   ! DLT
            end do                                                      ! DLT
         end do                                                         ! DLT
      end if                                                            ! DLT
C
C9------RETURN
      CALL GWF2LPF7PSV(IGRID)
      CALL SGWF2BAS7PSV(IGRID)                                          ! DLT
      RETURN
      END
      SUBROUTINE GWF2LPF7AD(KPER,IGRID)
C     ******************************************************************
C     SET HOLD TO BOTM WHENEVER A WETTABLE CELL IS DRY
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,ISSFLG,IBOUND,HOLD,BOTM,LBOTM
      USE GWFLPFMODULE,ONLY:LAYWET,WETDRY
C     ------------------------------------------------------------------
C
      CALL SGWF2LPF7PNT(IGRID)
      ISS=ISSFLG(KPER)
C
C1------RETURN IF STEADY STATE.
      IF(ISS.NE.0) RETURN
C
C2------LOOP THROUGH ALL LAYERS TO SET HOLD=BOT IF A WETTABLE CELL IS DRY
      ZERO=0.
      DO 100 K=1,NLAY
C
C2A-----SKIP LAYERS THAT CANNOT CONVERT BETWEEN WET AND DRY
      IF(LAYWET(K).EQ.0) GO TO 100
      DO 90 I=1,NROW
      DO 90 J=1,NCOL
C
C2B-----SKIP CELLS THAT ARE CURRENTLY WET OR ARE NOT WETTABLE
      IF(IBOUND(J,I,K).NE.0) GO TO 90
      IF(WETDRY(J,I,LAYWET(K)).EQ.ZERO) GO TO 90
C
C2C-----SET HOLD=BOT
      HOLD(J,I,K)=BOTM(J,I,LBOTM(K))
   90 CONTINUE
  100 CONTINUE
C
C3-----RETURN
      RETURN
      END
      SUBROUTINE GWF2LPF7FM(KITER,KSTP,KPER,IGRID)
C     ******************************************************************
C     ADD LEAKAGE CORRECTION AND STORAGE TO HCOF AND RHS, AND CALCULATE
C     CONDUCTANCE AS REQUIRED.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IBOUND,BOTM,NBOTM,DELR,DELC,
     1                      LBOTM,CV,HNEW,RHS,HCOF,HOLD,ISSFLG,IOUT
      USE GWFBASMODULE,ONLY:DELT
      USE GWFLPFMODULE,ONLY:LAYTYP,SC1,SC2,NOVFC
C     ------------------------------------------------------------------
C
C1------SET POINTERS TO DATA, GET STEADY-STATE FLAG FOR STRESS PERIOD,
C1------DEFINE CONSTANT.
      CALL SGWF2LPF7PNT(IGRID)
      ISS=ISSFLG(KPER)
      ONE=1.
C
C2------FOR EACH LAYER: IF CONVERTIBLE, CALCULATE CONDUCTANCES.
      DO 100 K=1,NLAY
      KK=K
      IF(LAYTYP(K).NE.0)
     1    CALL SGWF2LPF7HCOND(KK,KITER,KSTP,KPER)
  100 CONTINUE
      DO 101 K=1,NLAY
      KK=K
      IF(K.NE.NLAY) THEN
         IF(LAYTYP(K).NE.0 .OR. LAYTYP(K+1).NE.0)
     1      CALL SGWF2LPF7VCOND(KK)
      END IF
  101 CONTINUE
C
C3------IF THE STRESS PERIOD IS TRANSIENT, ADD STORAGE TO HCOF AND RHS
      IF(ISS.EQ.0) THEN
         TLED=ONE/DELT
         DO 200 K=1,NLAY
C
C4------SEE IF THIS LAYER IS CONVERTIBLE OR NON-CONVERTIBLE.
         IF(LAYTYP(K).EQ.0) THEN
C5------NON-CONVERTIBLE LAYER, SO USE PRIMARY STORAGE
            DO 140 I=1,NROW
            DO 140 J=1,NCOL
            IF(IBOUND(J,I,K).LE.0) GO TO 140
            RHO=SC1(J,I,K)*TLED
            HCOF(J,I,K)=HCOF(J,I,K)-RHO
            RHS(J,I,K)=RHS(J,I,K)-RHO*HOLD(J,I,K)
  140       CONTINUE
         ELSE
C
C6------A CONVERTIBLE LAYER, SO CHECK OLD AND NEW HEADS TO DETERMINE
C6------WHEN TO USE PRIMARY AND SECONDARY STORAGE
            DO 180 I=1,NROW
            DO 180 J=1,NCOL
C
C6A-----IF THE CELL IS EXTERNAL THEN SKIP IT.
            IF(IBOUND(J,I,K).LE.0) GO TO 180
            TP=BOTM(J,I,LBOTM(K)-1)
            RHO2=SC2(J,I,LAYTYP(K))*TLED
            RHO1=SC1(J,I,K)*TLED
C
C6B-----FIND STORAGE FACTOR AT START OF TIME STEP.
            SOLD=RHO2
            IF(HOLD(J,I,K).GT.TP) SOLD=RHO1
C
C6C-----FIND STORAGE FACTOR AT END OF TIME STEP.
            HTMP=HNEW(J,I,K)
            SNEW=RHO2
            IF(HTMP.GT.TP) SNEW=RHO1
C
C6D-----ADD STORAGE TERMS TO RHS AND HCOF.
            HCOF(J,I,K)=HCOF(J,I,K)-SNEW
            RHS(J,I,K)=RHS(J,I,K) - SOLD*(HOLD(J,I,K)-TP) - SNEW*TP
C
  180       CONTINUE
         END IF
C
  200    CONTINUE
      END IF
C
C7------FOR EACH LAYER DETERMINE IF CORRECTION TERMS ARE NEEDED FOR
C7------FLOW DOWN INTO PARTIALLY SATURATED LAYERS.
      IF(NOVFC.EQ.0) THEN
        DO 300 K=1,NLAY
C
C8------SEE IF CORRECTION IS NEEDED FOR LEAKAGE FROM ABOVE.
        IF(LAYTYP(K).NE.0 .AND. K.NE.1) THEN
C
C8A-----FOR EACH CELL MAKE THE CORRECTION IF NEEDED.
           DO 220 I=1,NROW
           DO 220 J=1,NCOL
C
C8B-----IF THE CELL IS EXTERNAL(IBOUND<=0) THEN SKIP IT.
           IF(IBOUND(J,I,K).LE.0) GO TO 220
           HTMP=HNEW(J,I,K)
C
C8C-----IF HEAD IS ABOVE TOP THEN CORRECTION NOT NEEDED
           TOP=BOTM(J,I,LBOTM(K)-1)
           IF(HTMP.GE.TOP) GO TO 220
C
C8D-----WITH HEAD BELOW TOP ADD CORRECTION TERMS TO RHS.
           RHS(J,I,K)=RHS(J,I,K) + CV(J,I,K-1)*(TOP-HTMP)
  220      CONTINUE
        END IF
C
C9------SEE IF THIS LAYER MAY NEED CORRECTION FOR LEAKAGE TO BELOW.
        IF(K.EQ.NLAY) GO TO 300
        IF(LAYTYP(K+1).NE.0) THEN
C
C9A-----FOR EACH CELL MAKE THE CORRECTION IF NEEDED.
           DO 280 I=1,NROW
           DO 280 J=1,NCOL
C
C9B-----IF CELL IS EXTERNAL (IBOUND<=0) THEN SKIP IT.
           IF(IBOUND(J,I,K).LE.0) GO TO 280
C
C9C-----IF HEAD IN THE LOWER CELL IS LESS THAN TOP ADD CORRECTION
C9C-----TERM TO RHS.
           HTMP=HNEW(J,I,K+1)
           TOP=BOTM(J,I,LBOTM(K+1)-1)
           IF(HTMP.LT.TOP) RHS(J,I,K)=RHS(J,I,K)- CV(J,I,K)*(TOP-HTMP)
  280      CONTINUE
        END IF
C
  300   CONTINUE
      END IF
C
C10-----RETURN
      RETURN
      END
      SUBROUTINE SGWF2LPF7N()
C     ******************************************************************
C     INITIALIZE AND CHECK LPF DATA
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IBOUND,HNEW,LAYCBD,CV,
     1                      BOTM,NBOTM,DELR,DELC,IOUT
      USE GWFBASMODULE,ONLY:HNOFLO
      USE GWFLPFMODULE,ONLY:LAYWET,WETDRY,HK,VKCB,LAYTYP,VKA
C     ------------------------------------------------------------------
C
C1------DEFINE CONSTANTS.
      ZERO=0.
      HCNV=HNOFLO
C
C2------INSURE THAT EACH ACTIVE CELL HAS AT LEAST ONE NON-ZERO
C2------TRANSMISSIVE PARAMETER.
      DO 60 K=1,NLAY
      IF(LAYWET(K).NE.0) THEN
C
C3------WETTING IS ACTIVE.
         DO 40 I=1,NROW
         DO 40 J=1,NCOL
         IF(IBOUND(J,I,K).EQ.0 .AND. WETDRY(J,I,LAYWET(K)).EQ.ZERO)
     1                GO TO 40
C
C3A-----CHECK HORIZONTAL HYDRAULIC CONDUCTIVITY (HK).
         IF(HK(J,I,K).NE.ZERO) GO TO 40
C
C3B-----CHECK VERTICAL HYDRAULIC CONDUCTIVITY AND CONFINING BED
C3B-----VERTICAL HYDRAULIC CONDUCTIVITY.
         IF(NLAY.GT.1) THEN

            IF(VKA(J,I,K).NE.ZERO) THEN
               IF(K.NE.NLAY) THEN
                  IF (VKA(J,I,K+1).NE.ZERO) THEN
                     IF(LAYCBD(K).NE.0) THEN
                        IF(VKCB(J,I,LAYCBD(K)).NE.ZERO) GO TO 40
                     ELSE
                        GO TO 40
                     END IF
                  END IF
               END IF
               IF(K.NE.1) THEN
                  IF (VKA(J,I,K-1).NE.ZERO) THEN
                     IF (LAYCBD(K-1).NE.0) THEN
                        IF(VKCB(J,I,LAYCBD(K-1)).NE.ZERO) GO TO 40
                     ELSE
                        GO TO 40
                     END IF
                  ENDIF
               END IF
            END IF
         END IF
C
C3C-----ALL TRANSMISSIVE TERMS ARE ALL 0, SO CONVERT CELL TO NO FLOW.
         IBOUND(J,I,K)=0
         HNEW(J,I,K)=HCNV
         WETDRY(J,I,LAYWET(K))=ZERO
         WRITE(IOUT,43) K,I,J
   40    CONTINUE
C
      ELSE
C
C4------WETTING IS INACTIVE
         DO 50 I=1,NROW
         DO 50 J=1,NCOL
         IF(IBOUND(J,I,K).EQ.0) GO TO 50
C
C4A-----CHECK HORIZONTAL HYDRAULIC CONDUCTIVITY (HK).
         IF(HK(J,I,K).NE.ZERO) GO TO 50
C
C4B-----CHECK VERTICAL HYDRAULIC CONDUCTIVITY AND CONFINING BED
C4B-----VERTICAL HYDRAULIC CONDUCTIVITY.
         IF(NLAY.GT.1) THEN
            IF(VKA(J,I,K).NE.ZERO) THEN
               IF(K.NE.NLAY) THEN
                  IF (VKA(J,I,K+1).NE.ZERO) THEN
                     IF(LAYCBD(K).NE.0) THEN
                        IF(VKCB(J,I,LAYCBD(K)).NE.ZERO) GO TO 50
                     ELSE
                        GO TO 50
                     END IF
                  END IF
               END IF
               IF(K.NE.1) THEN
                  IF (VKA(J,I,K-1).NE.ZERO) THEN
                     IF (LAYCBD(K-1).NE.0) THEN
                        IF(VKCB(J,I,LAYCBD(K-1)).NE.ZERO) GO TO 50
                     ELSE
                        GO TO 50
                     END IF
                  ENDIF
               END IF
            END IF
         END IF
C
C4C-----ALL TRANSMISSIVE TERMS ARE 0, SO CONVERT CELL TO NO FLOW.
         IBOUND(J,I,K)=0
         HNEW(J,I,K)=HCNV
         WRITE(IOUT,43) K,I,J
   43    FORMAT(1X,'NODE (LAYER,ROW,COL) ',I3,2(1X,I5),
     1 ' ELIMINATED BECAUSE ALL HYDRAULIC',/,
     2 ' CONDUCTIVITIES TO NODE ARE 0')
   50    CONTINUE
      END IF
   60 CONTINUE
C
C5------CALCULATE HOR. CONDUCTANCE(CR AND CC) FOR CONSTANT T LAYERS.
      DO 70 K=1,NLAY
      KK=K
      IF(LAYTYP(K).EQ.0) CALL SGWF2LPF7HCOND(KK,0,0,0)
   70 CONTINUE
C
C6------CALCULATE VERTICAL CONDUCTANCE BETWEEN CONFINED LAYERS.
      IF(NLAY.GT.1) THEN
         DO 10 K=1,NLAY-1
         KK=K
         IF(LAYTYP(K).EQ.0 .AND. LAYTYP(K+1).EQ.0)
     1      CALL SGWF2LPF7VCOND(KK)
   10    CONTINUE
      END IF
C
C7------RETURN.
      RETURN
      END
      SUBROUTINE GWF2LPF7BDADJ(KSTP,KPER,IDIR,IBDRET,
     1                      IC1,IC2,IR1,IR2,IL1,IL2,IGRID)
C     ******************************************************************
C     COMPUTE FLOW BETWEEN ADJACENT CELLS IN A SUBREGION OF THE GRID
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IBOUND,HNEW,BUFF,CR,CC,CV,
     1                      BOTM,LBOTM,IOUT
      USE GWFBASMODULE,ONLY:ICBCFL,DELT,PERTIM,TOTIM,ICHFLG
      USE GWFLPFMODULE,ONLY:ILPFCB,LAYTYP,NOVFC
C
      CHARACTER*16 TEXT(3)
      DOUBLE PRECISION HD
C
      DATA TEXT(1),TEXT(2),TEXT(3)
     1 /'FLOW RIGHT FACE ','FLOW FRONT FACE ','FLOW LOWER FACE '/
C     ------------------------------------------------------------------
C
      CALL SGWF2LPF7PNT(IGRID)
C
C1------IF CELL-BY-CELL FLOWS WILL BE SAVED IN A FILE, SET FLAG IBD.
C1------RETURN IF FLOWS ARE NOT BEING SAVED OR RETURNED.
      ZERO=0.
      IBD=0
      IF(ILPFCB.GT.0) IBD=ICBCFL
      IF(IBD.EQ.0 .AND. IBDRET.EQ.0) RETURN
C
C2------SET THE SUBREGION EQUAL TO THE ENTIRE GRID IF VALUES ARE BEING
C2------SAVED IN A FILE.
      IF(IBD.NE.0) THEN
         K1=1
         K2=NLAY
         I1=1
         I2=NROW
         J1=1
         J2=NCOL
      END IF
C
C3------TEST FOR DIRECTION OF CALCULATION;  IF NOT ACROSS COLUMNS, GO TO
C3------STEP 4.  IF ONLY 1 COLUMN, RETURN.
      IF(IDIR.NE.1) GO TO 405
      IF(NCOL.EQ.1) RETURN
C
C3A-----CALCULATE FLOW ACROSS COLUMNS (THROUGH RIGHT FACE).  IF NOT
C3A-----SAVING IN A FILE, SET THE SUBREGION.  CLEAR THE BUFFER.
      IF(IBD.EQ.0) THEN
         K1=IL1
         K2=IL2
         I1=IR1
         I2=IR2
         J1=IC1-1
         IF(J1.LT.1) J1=1
         J2=IC2
      END IF
      DO 310 K=K1,K2
      DO 310 I=I1,I2
      DO 310 J=J1,J2
      BUFF(J,I,K)=ZERO
  310 CONTINUE
C
C3B-----FOR EACH CELL CALCULATE FLOW THRU RIGHT FACE & STORE IN BUFFER.
      IF(J2.EQ.NCOL) J2=J2-1
      DO 400 K=K1,K2
      DO 400 I=I1,I2
      DO 400 J=J1,J2
      IF(ICHFLG.EQ.0) THEN
         IF((IBOUND(J,I,K).LE.0) .AND. (IBOUND(J+1,I,K).LE.0)) GO TO 400
      ELSE
         IF((IBOUND(J,I,K).EQ.0) .OR. (IBOUND(J+1,I,K).EQ.0)) GO TO 400
      END IF
      HDIFF=HNEW(J,I,K)-HNEW(J+1,I,K)
      BUFF(J,I,K)=HDIFF*CR(J,I,K)
  400 CONTINUE
C
C3C-----RECORD CONTENTS OF BUFFER AND RETURN.
      IF(IBD.EQ.1)
     1   CALL UBUDSV(KSTP,KPER,TEXT(1),ILPFCB,BUFF,NCOL,NROW,NLAY,IOUT)
      IF(IBD.EQ.2) CALL UBDSV1(KSTP,KPER,TEXT(1),ILPFCB,BUFF,NCOL,NROW,
     1     NLAY,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      RETURN
C
C4------TEST FOR DIRECTION OF CALCULATION;  IF NOT ACROSS ROWS, GO TO
C4------STEP 5.  IF ONLY 1 ROW, RETURN.
  405 IF(IDIR.NE.2) GO TO 505
      IF(NROW.EQ.1) RETURN
C
C4A-----CALCULATE FLOW ACROSS ROWS (THROUGH FRONT FACE).  IF NOT SAVING
C4A-----IN A FILE, SET THE SUBREGION.  CLEAR THE BUFFER.
      IF(IBD.EQ.0) THEN
         K1=IL1
         K2=IL2
         I1=IR1-1
         IF(I1.LT.1) I1=1
         I2=IR2
         J1=IC1
         J2=IC2
      END IF
      DO 410 K=K1,K2
      DO 410 I=I1,I2
      DO 410 J=J1,J2
      BUFF(J,I,K)=ZERO
  410 CONTINUE
C
C4B-----FOR EACH CELL CALCULATE FLOW THRU FRONT FACE & STORE IN BUFFER.
      IF(I2.EQ.NROW) I2=I2-1
      DO 500 K=K1,K2
      DO 500 I=I1,I2
      DO 500 J=J1,J2
      IF(ICHFLG.EQ.0) THEN
         IF((IBOUND(J,I,K).LE.0) .AND. (IBOUND(J,I+1,K).LE.0)) GO TO 500
      ELSE
         IF((IBOUND(J,I,K).EQ.0) .OR. (IBOUND(J,I+1,K).EQ.0)) GO TO 500
      END IF
      HDIFF=HNEW(J,I,K)-HNEW(J,I+1,K)
      BUFF(J,I,K)=HDIFF*CC(J,I,K)
  500 CONTINUE
C
C4C-----RECORD CONTENTS OF BUFFER AND RETURN.
      IF(IBD.EQ.1)
     1   CALL UBUDSV(KSTP,KPER,TEXT(2),ILPFCB,BUFF,NCOL,NROW,NLAY,IOUT)
      IF(IBD.EQ.2) CALL UBDSV1(KSTP,KPER,TEXT(2),ILPFCB,BUFF,NCOL,NROW,
     1     NLAY,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      RETURN
C
C5------DIRECTION OF CALCULATION IS ACROSS LAYERS BY ELIMINATION.  IF
C5------ONLY 1 LAYER, RETURN.
  505 IF(NLAY.EQ.1) RETURN
C
C5A-----CALCULATE FLOW ACROSS LAYERS (THROUGH LOWER FACE).  IF NOT
C5A-----SAVING IN A FILE, SET THE SUBREGION.  CLEAR THE BUFFER.
      IF(IBD.EQ.0) THEN
         K1=IL1-1
         IF(K1.LT.1) K1=1
         K2=IL2
         I1=IR1
         I2=IR2
         J1=IC1
         J2=IC2
      END IF
      DO 510 K=K1,K2
      DO 510 I=I1,I2
      DO 510 J=J1,J2
      BUFF(J,I,K)=ZERO
  510 CONTINUE
C
C5B-----FOR EACH CELL CALCULATE FLOW THRU LOWER FACE & STORE IN BUFFER.
      IF(K2.EQ.NLAY) K2=K2-1
      DO 600 K=1,K2
      IF(K.LT.K1) GO TO 600
      DO 590 I=I1,I2
      DO 590 J=J1,J2
      IF(ICHFLG.EQ.0) THEN
         IF((IBOUND(J,I,K).LE.0) .AND. (IBOUND(J,I,K+1).LE.0)) GO TO 590
      ELSE
         IF((IBOUND(J,I,K).EQ.0) .OR. (IBOUND(J,I,K+1).EQ.0)) GO TO 590
      END IF
      HD=HNEW(J,I,K+1)
      IF(NOVFC.NE.0 .OR. LAYTYP(K+1).EQ.0) GO TO 580
      TMP=HD
      TOP=BOTM(J,I,LBOTM(K+1)-1)
      IF(TMP.LT.TOP) HD=TOP
  580 HDIFF=HNEW(J,I,K)-HD
      BUFF(J,I,K)=HDIFF*CV(J,I,K)
  590 CONTINUE
  600 CONTINUE
C
C5C-----RECORD CONTENTS OF BUFFER AND RETURN.
      IF(IBD.EQ.1)
     1   CALL UBUDSV(KSTP,KPER,TEXT(3),ILPFCB,BUFF,NCOL,NROW,NLAY,IOUT)
      IF(IBD.EQ.2) CALL UBDSV1(KSTP,KPER,TEXT(3),ILPFCB,BUFF,NCOL,NROW,
     1     NLAY,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      RETURN
      END
      SUBROUTINE GWF2LPF7BDS(KSTP,KPER,IGRID)
C     ******************************************************************
C     COMPUTE STORAGE BUDGET FLOW TERM FOR LPF.
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,ISSFLG,IBOUND,HNEW,HOLD,
     1                      BUFF,BOTM,LBOTM,IOUT
      USE GWFBASMODULE,ONLY:MSUM,ICBCFL,VBVL,VBNM,DELT,PERTIM,TOTIM
      USE GWFLPFMODULE,ONLY:ILPFCB,LAYTYP,SC1,SC2

      CHARACTER*16 TEXT
      DOUBLE PRECISION STOIN,STOUT,SSTRG
C
      DATA TEXT /'         STORAGE'/
C     ------------------------------------------------------------------
C
      CALL SGWF2LPF7PNT(IGRID)
C
C1------INITIALIZE BUDGET ACCUMULATORS AND 1/DELT.
      ISS=ISSFLG(KPER)
      ZERO=0.
      STOIN=ZERO
      STOUT=ZERO
C2------IF STEADY STATE, STORAGE TERM IS ZERO
      IF(ISS.NE.0) GOTO 400
      ONE=1.
      TLED=ONE/DELT
C
C3------IF CELL-BY-CELL FLOWS WILL BE SAVED, SET FLAG IBD.
      IBD=0
      IF(ILPFCB.GT.0) IBD=ICBCFL
C
C4------CLEAR BUFFER.
      DO 210 K=1,NLAY
      DO 210 I=1,NROW
      DO 210 J=1,NCOL
      BUFF(J,I,K)=ZERO
210   CONTINUE
C
C5------LOOP THROUGH EVERY CELL IN THE GRID.
      KT=0
      DO 300 K=1,NLAY
      LC=LAYTYP(K)
      IF(LC.NE.0) KT=KT+1
      DO 300 I=1,NROW
      DO 300 J=1,NCOL
C
C6------SKIP NO-FLOW AND CONSTANT-HEAD CELLS.
      IF(IBOUND(J,I,K).LE.0) GO TO 300
      HSING=HNEW(J,I,K)
C
C7-----CHECK LAYER TYPE TO SEE IF ONE STORAGE CAPACITY OR TWO.
      IF(LC.EQ.0) GO TO 285
C
C7A----TWO STORAGE CAPACITIES.
      TP=BOTM(J,I,LBOTM(K)-1)
      RHO2=SC2(J,I,KT)*TLED
      RHO1=SC1(J,I,K)*TLED
      SOLD=RHO2
      IF(HOLD(J,I,K).GT.TP) SOLD=RHO1
      SNEW=RHO2
      IF(HSING.GT.TP) SNEW=RHO1
      STRG=SOLD*(HOLD(J,I,K)-TP) + SNEW*TP - SNEW*HSING
      GO TO 288
C
C7B----ONE STORAGE CAPACITY.
  285 RHO=SC1(J,I,K)*TLED
      STRG=RHO*HOLD(J,I,K) - RHO*HSING

C
C8-----STORE CELL-BY-CELL FLOW IN BUFFER AND ADD TO ACCUMULATORS.
  288 BUFF(J,I,K)=STRG
      SSTRG=STRG
      IF(STRG.LT.ZERO) THEN
        STOUT=STOUT-SSTRG
      ELSE
        STOIN=STOIN+SSTRG
      END IF
C
  300 CONTINUE
C
C9-----IF IBD FLAG IS SET RECORD THE CONTENTS OF THE BUFFER.
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,
     1                       ILPFCB,BUFF,NCOL,NROW,NLAY,IOUT)
      IF(IBD.EQ.2) CALL UBDSV1(KSTP,KPER,TEXT,ILPFCB,
     1            BUFF,NCOL,NROW,NLAY,IOUT,DELT,PERTIM,TOTIM,IBOUND)
C
C10-----ADD TOTAL RATES AND VOLUMES TO VBVL & PUT TITLE IN VBNM.
  400 CONTINUE
      SIN=STOIN
      SOUT=STOUT
      VBVL(1,MSUM)=VBVL(1,MSUM)+SIN*DELT
      VBVL(2,MSUM)=VBVL(2,MSUM)+SOUT*DELT
      VBVL(3,MSUM)=SIN
      VBVL(4,MSUM)=SOUT
      VBNM(MSUM)=TEXT
      MSUM=MSUM+1
C
C11----RETURN.
      RETURN
      END
      SUBROUTINE GWF2LPF7BDCH(KSTP,KPER,IGRID)
C     ******************************************************************
C     COMPUTE FLOW FROM CONSTANT-HEAD CELLS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IBOUND,HNEW,BUFF,CR,CC,CV,
     1                      BOTM,LBOTM,IOUT
      USE GWFBASMODULE,ONLY:MSUM,VBVL,VBNM,DELT,PERTIM,TOTIM,ICBCFL,
     1                      ICHFLG
      USE GWFLPFMODULE,ONLY:ILPFCB,LAYTYP,NOVFC

      CHARACTER*16 TEXT
      DOUBLE PRECISION HD,CHIN,CHOUT,XX1,XX2,XX3,XX4,XX5,XX6
C
      DATA TEXT /'   CONSTANT HEAD'/
C     ------------------------------------------------------------------
      CALL SGWF2LPF7PNT(IGRID)
C
C1------SET IBD TO INDICATE IF CELL-BY-CELL BUDGET VALUES WILL BE SAVED.
      IBD=0
      IF(ILPFCB.LT.0 .AND. ICBCFL.NE.0) IBD=-1
      IF(ILPFCB.GT.0) IBD=ICBCFL
C
C2------CLEAR BUDGET ACCUMULATORS.
      ZERO=0.
      CHIN=ZERO
      CHOUT=ZERO
      IBDLBL=0
C
C3------CLEAR BUFFER.
      DO 5 K=1,NLAY
      DO 5 I=1,NROW
      DO 5 J=1,NCOL
      BUFF(J,I,K)=ZERO
5     CONTINUE
C
C3A-----IF SAVING CELL-BY-CELL FLOW IN A LIST, COUNT CONSTANT-HEAD
C3A-----CELLS AND WRITE HEADER RECORDS.
      IF(IBD.EQ.2) THEN
         NCH=0
         DO 7 K=1,NLAY
         DO 7 I=1,NROW
         DO 7 J=1,NCOL
         IF(IBOUND(J,I,K).LT.0) NCH=NCH+1
7        CONTINUE
         CALL UBDSV2(KSTP,KPER,TEXT,ILPFCB,NCOL,NROW,NLAY,
     1          NCH,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      END IF
C
C4------LOOP THROUGH EACH CELL AND CALCULATE FLOW INTO MODEL FROM EACH
C4------CONSTANT-HEAD CELL.
      DO 200 K=1,NLAY
      DO 200 I=1,NROW
      DO 200 J=1,NCOL
C
C5------IF CELL IS NOT CONSTANT HEAD SKIP IT & GO ON TO NEXT CELL.
      IF (IBOUND(J,I,K).GE.0)GO TO 200
C
C6------CLEAR VALUES FOR FLOW RATE THROUGH EACH FACE OF CELL.
      X1=ZERO
      X2=ZERO
      X3=ZERO
      X4=ZERO
      X5=ZERO
      X6=ZERO
      CHCH1=ZERO
      CHCH2=ZERO
      CHCH3=ZERO
      CHCH4=ZERO
      CHCH5=ZERO
      CHCH6=ZERO
C
C7------CALCULATE FLOW THROUGH THE LEFT FACE.
C7------COMMENTS A-C APPEAR ONLY IN THE SECTION HEADED BY COMMENT 7,
C7------BUT THEY APPLY IN A SIMILAR MANNER TO SECTIONS 8-12.
C
C7A-----IF THERE IS NO FLOW TO CALCULATE THROUGH THIS FACE, THEN GO ON
C7A-----TO NEXT FACE.  NO FLOW OCCURS AT THE EDGE OF THE GRID, TO AN
C7A-----ADJACENT NO-FLOW CELL, OR TO AN ADJACENT CONSTANT-HEAD CELL.
      IF(J.EQ.1) GO TO 30
      IF(IBOUND(J-1,I,K).EQ.0) GO TO 30
      IF(IBOUND(J-1,I,K).LT.0 .AND. ICHFLG.EQ.0) GO TO 30
C
C7B-----CALCULATE FLOW THROUGH THIS FACE INTO THE ADJACENT CELL.
      HDIFF=HNEW(J,I,K)-HNEW(J-1,I,K)
      CHCH1=HDIFF*CR(J-1,I,K)
      IF(IBOUND(J-1,I,K).LT.0) GO TO 30
      X1=CHCH1
      XX1=X1
C
C7C-----ACCUMULATE POSITIVE AND NEGATIVE FLOW.
      IF(X1.LT.ZERO) THEN
        CHOUT=CHOUT-XX1
      ELSE
        CHIN=CHIN+XX1
      END IF
C
C8------CALCULATE FLOW THROUGH THE RIGHT FACE.
   30 IF(J.EQ.NCOL) GO TO 60
      IF(IBOUND(J+1,I,K).EQ.0) GO TO 60
      IF(IBOUND(J+1,I,K).LT.0 .AND. ICHFLG.EQ.0) GO TO 60
      HDIFF=HNEW(J,I,K)-HNEW(J+1,I,K)
      CHCH2=HDIFF*CR(J,I,K)
      IF(IBOUND(J+1,I,K).LT.0) GO TO 60
      X2=CHCH2
      XX2=X2
      IF(X2.LT.ZERO) THEN
        CHOUT=CHOUT-XX2
      ELSE
        CHIN=CHIN+XX2
      END IF
C
C9------CALCULATE FLOW THROUGH THE BACK FACE.
   60 IF(I.EQ.1) GO TO 90
      IF (IBOUND(J,I-1,K).EQ.0) GO TO 90
      IF (IBOUND(J,I-1,K).LT.0 .AND. ICHFLG.EQ.0) GO TO 90
      HDIFF=HNEW(J,I,K)-HNEW(J,I-1,K)
      CHCH3=HDIFF*CC(J,I-1,K)
      IF(IBOUND(J,I-1,K).LT.0) GO TO 90
      X3=CHCH3
      XX3=X3
      IF(X3.LT.ZERO) THEN
        CHOUT=CHOUT-XX3
      ELSE
        CHIN=CHIN+XX3
      END IF
C
C10-----CALCULATE FLOW THROUGH THE FRONT FACE.
   90 IF(I.EQ.NROW) GO TO 120
      IF(IBOUND(J,I+1,K).EQ.0) GO TO 120
      IF(IBOUND(J,I+1,K).LT.0 .AND. ICHFLG.EQ.0) GO TO 120
      HDIFF=HNEW(J,I,K)-HNEW(J,I+1,K)
      CHCH4=HDIFF*CC(J,I,K)
      IF(IBOUND(J,I+1,K).LT.0) GO TO 120
      X4=CHCH4
      XX4=X4
      IF(X4.LT.ZERO) THEN
        CHOUT=CHOUT-XX4
      ELSE
        CHIN=CHIN+XX4
      END IF
C
C11-----CALCULATE FLOW THROUGH THE UPPER FACE.
  120 IF(K.EQ.1) GO TO 150
      IF (IBOUND(J,I,K-1).EQ.0) GO TO 150
      IF (IBOUND(J,I,K-1).LT.0 .AND. ICHFLG.EQ.0) GO TO 150
      HD=HNEW(J,I,K)
      IF(NOVFC.NE.0 .OR. LAYTYP(K).EQ.0) GO TO 122
      TMP=HD
      TOP=BOTM(J,I,LBOTM(K)-1)
      IF(TMP.LT.TOP) HD=TOP
  122 HDIFF=HD-HNEW(J,I,K-1)
      CHCH5=HDIFF*CV(J,I,K-1)
      IF(IBOUND(J,I,K-1).LT.0) GO TO 150
      X5=CHCH5
      XX5=X5
      IF(X5.LT.ZERO) THEN
        CHOUT=CHOUT-XX5
      ELSE
        CHIN=CHIN+XX5
      END IF
C
C12-----CALCULATE FLOW THROUGH THE LOWER FACE.
  150 IF(K.EQ.NLAY) GO TO 180
      IF(IBOUND(J,I,K+1).EQ.0) GO TO 180
      IF(IBOUND(J,I,K+1).LT.0 .AND. ICHFLG.EQ.0) GO TO 180
      HD=HNEW(J,I,K+1)
      IF(NOVFC.NE.0 .OR. LAYTYP(K+1).EQ.0) GO TO 152
      TMP=HD
      TOP=BOTM(J,I,LBOTM(K+1)-1)
      IF(TMP.LT.TOP) HD=TOP
  152 HDIFF=HNEW(J,I,K)-HD
      CHCH6=HDIFF*CV(J,I,K)
      IF(IBOUND(J,I,K+1).LT.0) GO TO 180
      X6=CHCH6
      XX6=X6
      IF(X6.LT.ZERO) THEN
        CHOUT=CHOUT-XX6
      ELSE
        CHIN=CHIN+XX6
      END IF
C
C13-----SUM THE FLOWS THROUGH SIX FACES OF CONSTANT HEAD CELL, AND
C13-----STORE SUM IN BUFFER.
 180  RATE=CHCH1+CHCH2+CHCH3+CHCH4+CHCH5+CHCH6
      BUFF(J,I,K)=RATE
C
C14-----PRINT THE FLOW FOR THE CELL IF REQUESTED.
      IF(IBD.LT.0) THEN
         IF(IBDLBL.EQ.0) WRITE(IOUT,899) TEXT,KPER,KSTP
  899    FORMAT(1X,/1X,A,'   PERIOD ',I4,'   STEP ',I3)
         WRITE(IOUT,900) K,I,J,RATE
  900    FORMAT(1X,'LAYER ',I3,'   ROW ',I5,'   COL ',I5,
     1       '   RATE ',1PG15.6)
         IBDLBL=1
      END IF
C
C15-----IF SAVING CELL-BY-CELL FLOW IN LIST, WRITE FLOW FOR CELL.
      IF(IBD.EQ.2) CALL UBDSVA(ILPFCB,NCOL,NROW,J,I,K,RATE,IBOUND,NLAY)
  200 CONTINUE
C
C16-----IF SAVING CELL-BY-CELL FLOW IN 3-D ARRAY, WRITE THE ARRAY.
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,
     1                   ILPFCB,BUFF,NCOL,NROW,NLAY,IOUT)
C
C17-----SAVE TOTAL CONSTANT HEAD FLOWS AND VOLUMES IN VBVL TABLE
C17-----FOR INCLUSION IN BUDGET. PUT LABELS IN VBNM TABLE.
      CIN=CHIN
      COUT=CHOUT
      VBVL(1,MSUM)=VBVL(1,MSUM)+CIN*DELT
      VBVL(2,MSUM)=VBVL(2,MSUM)+COUT*DELT
      VBVL(3,MSUM)=CIN
      VBVL(4,MSUM)=COUT
      VBNM(MSUM)=TEXT
      MSUM=MSUM+1
C
C18-----RETURN.
      RETURN
      END
      SUBROUTINE SGWF2LPF7SC(SC,K,ISPST)
C     ******************************************************************
C     COMPUTE STORAGE CAPACITY
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,        ONLY:NCOL,NROW,DELR,DELC,BOTM,LBOTM,LAYCBD
C
      DIMENSION SC(NCOL,NROW)
C     ------------------------------------------------------------------
C
C1------MULTIPLY SPECIFIC STORAGE BY THICKNESS, DELR, AND DELC TO GET
C1------CONFINED STORAGE CAPACITY.
      IF(ISPST.NE.0) THEN
         DO 80 I=1,NROW
         DO 80 J=1,NCOL
         THICK=BOTM(J,I,LBOTM(K)-1)-BOTM(J,I,LBOTM(K))
         SC(J,I)=SC(J,I)*THICK*DELR(J)*DELC(I)
   80    CONTINUE
      ELSE
C
C2------MULTIPLY SPECIFIC YIELD BY DELR AND DELC TO GET UNCONFINED
C2------STORAGEE CAPACITY(SC2).
         DO 85 I=1,NROW
         DO 85 J=1,NCOL
         SC(J,I)=SC(J,I)*DELR(J)*DELC(I)
   85    CONTINUE
      END IF
C
      RETURN
      END
      SUBROUTINE SGWF2LPF7HCOND(K,KITER,KSTP,KPER)
C     ******************************************************************
C     COMPUTE HORIZONTAL BRANCH CONDUCTANCE FOR ONE LAYER.
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,IBOUND,HNEW,BOTM,NBOTM,
     1                      LBOTM,CC,STRT
      USE GWFBASMODULE,ONLY:HDRY
      USE GWFLPFMODULE,ONLY:LAYWET,IWETIT,LAYTYP,LAYAVG,LAYSTRT,
     1                      IMINKD                                      ! DLT
C
      CHARACTER*3 ACNVRT
      DIMENSION ICNVRT(5),JCNVRT(5),ACNVRT(5)
C
C     ------------------------------------------------------------------
C1------INITIALIZE DATA.
      ZERO=0.
      NCNVRT=0
      IHDCNV=0
C
C2------IF LAYER IS WETTABLE CONVERT DRY CELLS TO WET WHEN APPROPRIATE.
      ITFLG=1
      IF(LAYWET(K).NE.0) ITFLG=MOD(KITER,IWETIT)
      IF(ITFLG.EQ.0) CALL SGWF2LPF7WET(K,KITER,KSTP,KPER,
     2      IHDCNV,NCNVRT,ICNVRT,JCNVRT,ACNVRT)
C
C3------LOOP THROUGH EACH CELL, AND CALCULATE SATURATED THICKNESS.
      DO 200 I=1,NROW
      DO 200 J=1,NCOL
C
C3A-----SET STAURATED THICKNESS=0. FOR DRY CELLS.
      IF(IBOUND(J,I,K).EQ.0) THEN
         CC(J,I,K)=ZERO
      ELSE
C
C3B-----CALCULATE SATURATED THICKNESS FOR A WET CELL.
         BBOT=BOTM(J,I,LBOTM(K))
         IF(LAYSTRT(K).NE.0) THEN
            TTOP=STRT(J,I,K)
            IF(BBOT.GT.TTOP) THEN
               WRITE(IOUT,33) K,I,J
   33       FORMAT(1X,/1X,'Negative cell thickness at (layer,row,col)',
     1         I4,',',I5,',',I5)
               WRITE(IOUT,34) TTOP,BBOT
   34          FORMAT(1X,'Initial head, bottom elevation:',1P,2G13.5)
               CALL USTOP(' ')
            END IF
         ELSE
            TTOP=BOTM(J,I,LBOTM(K)-1)
            IF(BBOT.GT.TTOP) THEN
               WRITE(IOUT,35) K,I,J
   35       FORMAT(1X,/1X,'Negative cell thickness at (layer,row,col)',
     1         I4,',',I5,',',I5)
               WRITE(IOUT,36) TTOP,BBOT
   36          FORMAT(1X,'Top elevation, bottom elevation:',1P,2G13.5)
               CALL USTOP(' ')
            END IF
         END IF
         IF(LAYTYP(K).NE.0) THEN
            HHD=HNEW(J,I,K)
            IF(HHD.LT.TTOP) TTOP=HHD
         END IF
         THCK=TTOP-BBOT
         CC(J,I,K)=THCK
C
C
C3C-----WHEN SATURATED THICKNESS <= 0, PRINT A MESSAGE AND SET
C3C-----HNEW=HDRY, SATURATED THICKNESS=0.0, AND IBOUND=0.
         IF(THCK.LE.ZERO .AND. IMINKD.NE.1) THEN                        ! DLT
            CALL SGWF2LPF7WDMSG(1,NCNVRT,ICNVRT,JCNVRT,ACNVRT,IHDCNV,
     1             IOUT,KITER,J,I,K,KSTP,KPER,NCOL,NROW)
            HNEW(J,I,K)=HDRY
            CC(J,I,K)=ZERO
            IF(IBOUND(J,I,K).LT.0) THEN
               WRITE(IOUT,151)
  151          FORMAT(1X,/1X,'CONSTANT-HEAD CELL WENT DRY',
     1          ' -- SIMULATION ABORTED')
                    WRITE(IOUT,*) TTOP, BBOT, THCK
               WRITE(IOUT,152) K,I,J,KITER,KSTP,KPER
  152          FORMAT(1X,'LAYER=',I3,' ROW=',I5,' COLUMN=',I5,
     1    ' ITERATION=',I3,' TIME STEP=',I3,' STRESS PERIOD=',I4)
               CALL USTOP(' ')
            END IF
            IBOUND(J,I,K)=0
         END IF
      END IF
  200 CONTINUE
C
C4------PRINT ANY REMAINING CELL CONVERSIONS NOT YET PRINTED.
      CALL SGWF2LPF7WDMSG(0,NCNVRT,ICNVRT,JCNVRT,ACNVRT,IHDCNV,
     1             IOUT,KITER,J,I,K,KSTP,KPER,NCOL,NROW)
C
C5------CHANGE IBOUND VALUE FOR CELLS THAT CONVERTED TO WET THIS
C5------ITERATION FROM 30000 to 1.
      IF(LAYWET(K).NE.0) THEN
         DO 205 I=1,NROW
         DO 205 J=1,NCOL
         IF(IBOUND(J,I,K).EQ.30000) IBOUND(J,I,K)=1
  205    CONTINUE
      END IF
C
C6------COMPUTE HORIZONTAL BRANCH CONDUCTANCES FROM CELL HYDRAULIC
C6------CONDUCTIVITY, SATURATED THICKNESS, AND GRID DIMENSIONS.
      IF(LAYAVG(K).EQ.0) THEN
         CALL SGWF2LPF7HHARM(K)
      ELSE IF(LAYAVG(K).EQ.1) THEN
         CALL SGWF2LPF7HLOG(K)
      ELSE
         CALL SGWF2LPF7HUNCNF(K)
      END IF
C
C7------RETURN.
      RETURN
      END
      SUBROUTINE SGWF2LPF7WET(K,KITER,KSTP,KPER,IHDCNV,NCNVRT,
     1                        ICNVRT,JCNVRT,ACNVRT)
C
C     ******************************************************************
C     CONVERT DRY CELLS TO WET.
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,        ONLY:IOUT,NCOL,NROW,NLAY,HNEW,IBOUND,BOTM,LBOTM
      USE GWFLPFMODULE,  ONLY:LAYTYP,CHANI,LAYVKA,LAYWET,WETDRY,
     1                        WETFCT,IHDWET
C
      CHARACTER*3 ACNVRT
      DIMENSION ICNVRT(5),JCNVRT(5),ACNVRT(5)
C     ------------------------------------------------------------------
C
C1------LOOP THROUGH ALL CELLS.
      ZERO=0.0
      DO 100 I=1,NROW
      DO 100 J=1,NCOL
C
C2------IF CELL IS DRY AND IF IT IS WETTABLE, CONTINUE CHECKING TO SEE
C2------IF IT SHOULD BECOME WET.
      IF(IBOUND(J,I,K).EQ.0 .AND. WETDRY(J,I,LAYWET(K)).NE.ZERO) THEN
C
C3------CALCULATE WETTING ELEVATION.
         WD=WETDRY(J,I,LAYWET(K))
         IF(WD.LT.ZERO) WD=-WD
         TURNON=BOTM(J,I,LBOTM(K))+WD
C
C4------CHECK HEAD IN CELL BELOW TO SEE IF WETTING ELEVATION HAS BEEN
C4------REACHED.
         IF(K.NE.NLAY) THEN
            HTMP=HNEW(J,I,K+1)
            IF(IBOUND(J,I,K+1).GT.0 .AND. HTMP.GE.TURNON) GO TO 50
         END IF
C
C5------CHECK HEAD IN ADJACENT HORIZONTAL CELLS TO SEE IF WETTING
C5------ELEVATION HAS BEEN REACHED.
         IF(WETDRY(J,I,LAYWET(K)).GT.ZERO) THEN
            IF(J.NE.1) THEN
               HTMP=HNEW(J-1,I,K)
               IF(IBOUND(J-1,I,K).GT.0 .AND. IBOUND(J-1,I,K).NE.30000.
     1                       AND. HTMP.GE.TURNON) GO TO 50
            END IF
            IF(J.NE.NCOL) THEN
               HTMP=HNEW(J+1,I,K)
               IF(IBOUND(J+1,I,K).GT.0 .AND. HTMP.GE.TURNON) GO TO 50
            END IF
            IF(I.NE.1) THEN
               HTMP=HNEW(J,I-1,K)
               IF(IBOUND(J,I-1,K).GT.0 .AND. IBOUND(J,I-1,K).NE.30000.
     1                       AND. HTMP.GE.TURNON) GO TO 50
            END IF
            IF(I.NE.NROW) THEN
               HTMP=HNEW(J,I+1,K)
               IF(IBOUND(J,I+1,K).GT.0 .AND. HTMP.GE.TURNON) GO TO 50
            END IF
         END IF
C
C6------WETTING ELEVATION HAS NOT BEEN REACHED, SO CELL REMAINS DRY.
         GO TO 100
C
C7------CELL BECOMES WET.  PRINT MESSAGE, SET INITIAL HEAD, AND SET
C7------IBOUND.
   50    CALL SGWF2LPF7WDMSG(2,NCNVRT,ICNVRT,JCNVRT,ACNVRT,IHDCNV,
     1             IOUT,KITER,J,I,K,KSTP,KPER,NCOL,NROW)
C
C7A-----USE EQUATION 3A IF IHDWET=0; USE EQUATION 3B IF IHDWET IS NOT 0.
         IF(IHDWET.EQ.0) THEN
            HNEW(J,I,K)=BOTM(J,I,LBOTM(K))+
     1                        WETFCT*(HTMP-BOTM(J,I,LBOTM(K)))
         ELSE
            HNEW(J,I,K)=BOTM(J,I,LBOTM(K))+WETFCT*WD
         END IF
         IBOUND(J,I,K)=30000
      END IF
C
C8------END OF LOOP FOR ALL CELLS IN LAYER.
  100 CONTINUE
C
C9------RETURN.
      RETURN
      END
      SUBROUTINE SGWF2LPF7WDMSG(ICODE,NCNVRT,ICNVRT,JCNVRT,ACNVRT,
     1             IHDCNV,IOUT,KITER,J,I,K,KSTP,KPER,NCOL,NROW)
C     ******************************************************************
C     PRINT MESSAGE WHEN CELLS CONVERT BETWEEN WET AND DRY.
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*3 ACNVRT
      DIMENSION ICNVRT(5),JCNVRT(5),ACNVRT(5)
C     ------------------------------------------------------------------
C
C1------KEEP TRACK OF CELL CONVERSIONS.
      IF(ICODE.GT.0) THEN
         NCNVRT=NCNVRT+1
         ICNVRT(NCNVRT)=I
         JCNVRT(NCNVRT)=J
         IF(ICODE.EQ.1) THEN
            ACNVRT(NCNVRT)='DRY'
         ELSE
            ACNVRT(NCNVRT)='WET'
         END IF
      END IF
C
C2------PRINT A LINE OF DATA IF 5 CONVERSIONS HAVE OCCURRED OR IF ICODE
C2------INDICATES THAT A PARTIAL LINE SHOULD BE PRINTED.
      IF(NCNVRT.EQ.5 .OR. (ICODE.EQ.0 .AND. NCNVRT.GT.0)) THEN
         IF(IHDCNV.EQ.0) WRITE(IOUT,17) KITER,K,KSTP,KPER
   17    FORMAT(1X,/1X,'CELL CONVERSIONS FOR ITER.=',I3,'  LAYER=',
     1       I3,'  STEP=',I3,'  PERIOD=',I4,'   (ROW,COL)')
         IHDCNV=1
         IF (NROW.LE.999 .AND. NCOL.LE.999) THEN
           WRITE(IOUT,18) (ACNVRT(L),ICNVRT(L),JCNVRT(L),L=1,NCNVRT)
   18      FORMAT(1X,3X,5(A,'(',I3,',',I3,')   '))
         ELSE
           WRITE(IOUT,19) (ACNVRT(L),ICNVRT(L),JCNVRT(L),L=1,NCNVRT)
   19      FORMAT(1X,2X,5(A,'(',I5,',',I5,')'))
         ENDIF
         NCNVRT=0
      END IF
C
C3------RETURN.
      RETURN
      END
      SUBROUTINE SGWF2LPF7HHARM(K)
C     ******************************************************************
C     COMPUTE HORIZONTAL BRANCH CONDUCTANCE USING HARMONIC MEAN OF BLOCK
C     CONDUCTANCES (DISTANCE WEIGHTED HARMONIC MEAN OF TRANSMISSIVITY).
C     CELL THICKNESS IS IN CC UPON ENTRY.
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,IBOUND,CR,CC,DELR,DELC
      USE GWFLPFMODULE,ONLY:HK,CHANI,HANI,
     1                      IMINKD,MINKD                                ! DLT
C     ------------------------------------------------------------------
C
      ZERO=0.
      TWO=2.
C
C1------FOR EACH CELL CALCULATE BRANCH CONDUCTANCES FROM THAT CELL
C1------TO THE ONE ON THE RIGHT AND THE ONE IN FRONT.
      DO 100 I=1,NROW
      DO 100 J=1,NCOL
C
C2------IF CELL IS DRY OR HK=0., SET CONDUCTANCE EQUAL TO 0 AND GO ON
C2------TO NEXT CELL.
      IF(IBOUND(J,I,K).EQ.0 .OR. HK(J,I,K).EQ.ZERO) THEN
         CR(J,I,K)=ZERO
         CC(J,I,K)=ZERO
      ELSE
C
C3------CELL IS WET -- CALCULATE TRANSMISSIVITY OF CELL.
         T1=HK(J,I,K)*CC(J,I,K)
C3A-----IF THIS IS NOT THE LAST COLUMN (RIGHTMOST), CALCULATE
C3A-----BRANCH CONDUCTANCE IN THE ROW DIRECTION (CR) TO THE RIGHT.
         IF(J.NE.NCOL) THEN
            IF(IBOUND(J+1,I,K).NE.0) THEN
               T2=HK(J+1,I,K)*CC(J+1,I,K)
               IF (IMINKD.EQ.1) THEN                                    ! DLT
                  T1 = MAX(T1,MINKD)                                    ! DLT
                  T2 = MAX(T2,MINKD)                                    ! DLT
               END IF                                                   ! DLT
               IF(T1+T2.GT.0.0)THEN                                     ! DLT
                CR(J,I,K)=TWO*T2*T1*DELC(I)/(T1*DELR(J+1)+T2*DELR(J))
               ELSE                                                     ! DLT
                CR(J,I,K)=ZERO                                          ! DLT
               ENDIF                                                    ! DLT
            ELSE
               CR(J,I,K)=ZERO
            END IF
         ELSE
C3B-----IF THIS IS THE LAST COLUMN, SET BRANCH CONDUCTANCE=0.
            CR(J,I,K)=ZERO
         END IF
C
C3C-----IF THIS IS NOT THE LAST ROW (FRONTMOST) THEN CALCULATE
C3C-----BRANCH CONDUCTANCE IN THE COLUMN DIRECTION (CC) TO THE FRONT.
         IF(I.NE.NROW) THEN
            IF(IBOUND(J,I+1,K).NE.0) THEN
               T2=HK(J,I+1,K)*CC(J,I+1,K)
               IF(CHANI(K).LE.ZERO) THEN
                  KHANI=-CHANI(K)
                  T1=T1*HANI(J,I,KHANI)
                  T2=T2*HANI(J,I+1,KHANI)
               ELSE
                  T1=T1*CHANI(K)
                  T2=T2*CHANI(K)
               END IF
               IF (IMINKD.EQ.1) THEN                                    ! DLT
                  T1 = MAX(T1,MINKD)                                    ! DLT
                  T2 = MAX(T2,MINKD)                                    ! DLT
               END IF                                                   ! DLT
               IF(T1+T2.GT.0.0)THEN                                     ! DLT
                CC(J,I,K)=TWO*T2*T1*DELR(J)/(T1*DELC(I+1)+T2*DELC(I))
               ELSE                                                     ! DLT
                CC(J,I,K)=ZERO                                          ! DLT
               ENDIF                                                    ! DLT
            ELSE
C3D-----IF THIS IS THE LAST ROW, SET BRANCH CONDUCTANCE=0.
               CC(J,I,K)=ZERO
            END IF
         ELSE
            CC(J,I,K)=ZERO
         END IF
      END IF
  100 CONTINUE
C
C4------RETURN
      RETURN
      END
      SUBROUTINE SGWF2LPF7HLOG(K)
C     ******************************************************************
C-----COMPUTE HORIZONTAL CONDUCTANCE USING LOGARITHMIC MEAN
C-----TRANSMISSIVITY -- ACTIVATED BY LAYAVG=1
C-----CELL SATURATED THICKNESS IS IN CC.
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,IBOUND,CR,CC,DELR,DELC
      USE GWFLPFMODULE,ONLY:HK,CHANI,HANI
C     ------------------------------------------------------------------
C
      ZERO=0.
      TWO=2.
      HALF=0.5
      FRAC1=1.005
      FRAC2=0.995
C
C1------FOR EACH CELL CALCULATE BRANCH CONDUCTANCES FROM THAT CELL
C1------TO THE ONE ON THE RIGHT AND THE ONE IN FRONT.
      DO 100 I=1,NROW
      DO 100 J=1,NCOL
C
C2------IF CELL IS DRY OR HK=0., SET CONDUCTANCE EQUAL TO 0 AND GO ON
C2------TO NEXT CELL.
      IF(IBOUND(J,I,K).EQ.0 .OR. HK(J,I,K).EQ.ZERO) THEN
         CR(J,I,K)=ZERO
         CC(J,I,K)=ZERO
      ELSE
C
C3------CELL IS WET -- CALCULATE TRANSMISSIVITY OF CELL.
         T1=HK(J,I,K)*CC(J,I,K)
C3A-----IF THIS IS NOT THE LAST COLUMN(RIGHTMOST) THEN CALCULATE
C3A-----BRANCH CONDUCTANCE IN THE ROW DIRECTION (CR) TO THE RIGHT.
         IF(J.NE.NCOL) THEN
            IF(IBOUND(J+1,I,K).NE.0) THEN
C3A1----LOGARITHMIC MEAN INTERBLOCK TRANSMISSIVITY
               T2=HK(J+1,I,K)*CC(J+1,I,K)
               RATIO=T2/T1
               IF(RATIO.GT.FRAC1 .OR. RATIO.LT.FRAC2) THEN
                  T=(T2-T1)/LOG(RATIO)
               ELSE
                  T=HALF*(T1+T2)
               END IF
               CR(J,I,K)=TWO*DELC(I)*T/(DELR(J+1)+DELR(J))
            ELSE
               CR(J,I,K)=ZERO
            END IF
         ELSE
            CR(J,I,K)=ZERO
         END IF
C
C3B-----IF THIS IS NOT THE LAST ROW (FRONTMOST) THEN CALCULATE
C3B-----BRANCH CONDUCTANCE IN THE COLUMN DIRECTION (CC) TO THE FRONT.
         IF(I.NE.NROW) THEN
            IF(IBOUND(J,I+1,K).NE.0) THEN
               T2=HK(J,I+1,K)*CC(J,I+1,K)
               IF(CHANI(K).LE.ZERO) THEN
                  KHANI=-CHANI(K)
                  T1=T1*HANI(J,I,KHANI)
                  T2=T2*HANI(J,I+1,KHANI)
               ELSE
                  T1=T1*CHANI(K)
                  T2=T2*CHANI(K)
               END IF
               RATIO=T2/T1
               IF(RATIO.GT.FRAC1 .OR. RATIO.LT.FRAC2) THEN
                  T=(T2-T1)/LOG(RATIO)
               ELSE
                  T=HALF*(T1+T2)
               END IF
               CC(J,I,K)=TWO*DELR(J)*T/(DELC(I+1)+DELC(I))
            ELSE
               CC(J,I,K)=ZERO
            END IF
         ELSE
            CC(J,I,K)=ZERO
         END IF
      END IF
  100 CONTINUE
C
C4------RETURN
      RETURN
      END
      SUBROUTINE SGWF2LPF7HUNCNF(K)
C     ******************************************************************
C-----COMPUTE HORIZONTAL CONDUCTANCE USING ARITHMETIC MEAN SATURATED
C-----THICKNESS AND LOGARITHMIC MEAN HYDRAULIC CONDUCTIVITY.
C-----CELL SATURATED THICKNESS IS IN CC.
C-----ACTIVATED BY LAYAVG=2
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,IBOUND,CR,CC,DELR,DELC
      USE GWFLPFMODULE,ONLY:HK,CHANI,HANI
C     ------------------------------------------------------------------
C
      ZERO=0.
      HALF=0.5
      FRAC1=1.005
      FRAC2=0.995
C
C1------FOR EACH CELL CALCULATE BRANCH CONDUCTANCES FROM THAT CELL
C1------TO THE ONE ON THE RIGHT AND THE ONE IN FRONT.
      DO 100 I=1,NROW
      DO 100 J=1,NCOL
C
C2------IF CELL IS DRY OR HK=0., SET CONDUCTANCE EQUAL TO 0 AND GO ON
C2------TO NEXT CELL.
      IF(IBOUND(J,I,K).EQ.0 .OR. HK(J,I,K).EQ.ZERO) THEN
         CR(J,I,K)=ZERO
         CC(J,I,K)=ZERO
      ELSE
C
C3------CELL IS WET -- CALCULATE TRANSMISSIVITY OF CELL.
         HYC1=HK(J,I,K)
C3A-----IF THIS IS NOT THE LAST COLUMN(RIGHTMOST) THEN CALCULATE
C3A-----BRANCH CONDUCTANCE IN THE ROW DIRECTION (CR) TO THE RIGHT.
         IF(J.NE.NCOL) THEN
            IF(IBOUND(J+1,I,K).NE.0) THEN
C3A1----LOGARITHMIC MEAN HYDRAULIC CONDUCTIVITY
               HYC2=HK(J+1,I,K)
               RATIO=HYC2/HYC1
               IF(RATIO.GT.FRAC1 .OR. RATIO.LT.FRAC2) THEN
                  HYC=(HYC2-HYC1)/LOG(RATIO)
               ELSE
                  HYC=HALF*(HYC1+HYC2)
               END IF
C3A2----MULTIPLY LOGARITHMIC K BY ARITMETIC SATURATED THICKNESS.
               CR(J,I,K)=DELC(I)*HYC*(CC(J,I,K)+CC(J+1,I,K))/
     1                      (DELR(J+1)+DELR(J))
            ELSE
               CR(J,I,K)=ZERO
            END IF
         ELSE
            CR(J,I,K)=ZERO
         END IF
C
C3B-----IF THIS IS NOT THE LAST ROW (FRONTMOST) THEN CALCULATE
C3B-----BRANCH CONDUCTANCE IN THE COLUMN DIRECTION (CC) TO THE FRONT.
         IF(I.NE.NROW) THEN
            IF(IBOUND(J,I+1,K).NE.0) THEN
C3B1----LOGARITHMIC MEAN HYDRAULIC CONDUCTIVITY
               HYC2=HK(J,I+1,K)
               IF(CHANI(K).LE.ZERO) THEN
                  KHANI=-CHANI(K)
                  HYC1=HYC1*HANI(J,I,KHANI)
                  HYC2=HYC2*HANI(J,I+1,KHANI)
               ELSE
                  HYC1=HYC1*CHANI(K)
                  HYC2=HYC2*CHANI(K)
               END IF
               RATIO=HYC2/HYC1
               IF(RATIO.GT.FRAC1 .OR. RATIO.LT.FRAC2) THEN
                  HYC=(HYC2-HYC1)/LOG(RATIO)
               ELSE
                  HYC=HALF*(HYC1+HYC2)
               END IF
C3B2----MULTIPLY LOGARITHMIC K BY ARITMETIC SATURATED THICKNESS.
               CC(J,I,K)=DELR(J)*HYC*(CC(J,I,K)+CC(J,I+1,K))/
     1                      (DELC(I+1)+DELC(I))
            ELSE
               CC(J,I,K)=ZERO
            END IF
         ELSE
            CC(J,I,K)=ZERO
         END IF
      END IF
  100 CONTINUE
C
C4------RETURN.
      RETURN
      END
      SUBROUTINE SGWF2LPF7VCOND(K)
C     ******************************************************************
C     COMPUTE VERTICAL BRANCH CONDUCTANCE BETWEEN A LAYER AND THE NEXT
C     LOWER LAYER FROM VERTICAL HYDRAULIC CONDUCTIVITY.
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,        ONLY:NCOL,NROW,NLAY,IBOUND,HNEW,CV,DELR,DELC,
     1                        BOTM,LBOTM,LAYCBD,IOUT,STRT
      USE GWFLPFMODULE,  ONLY:LAYTYP,LAYAVG,CHANI,LAYVKA,LAYWET,
     1                        HK,VKA,VKCB,NOCVCO,ICONCV,LAYSTRT,
     2                        IMINC,MINC                                ! DLT
C
      DOUBLE PRECISION BBOT,TTOP,HHD
      REAL :: SUMVAL, MAXC, TINY                                        ! DLT
      PARAMETER( MAXC = 1.0E6,TINY=1.0E-20)                             ! DLT
C     ------------------------------------------------------------------
C
      IF(K.EQ.NLAY) RETURN
      ZERO=0.
      HALF=0.5
C
C1------LOOP THROUGH ALL CELLS IN THE LAYER.
      DO 100 I=1,NROW
      DO 100 J=1,NCOL
      CV(J,I,K)=ZERO
      IF(IBOUND(J,I,K).NE.0 .AND. IBOUND(J,I,K+1).NE.0) THEN
C
C2------CALCULATE VERTICAL HYDRAULIC CONDUCTIVITY FOR CELL.
         IF(LAYVKA(K).EQ.0) THEN
            HYC1=VKA(J,I,K)
         ELSE
            HYC1=HK(J,I,K)/VKA(J,I,K)
         END IF
         IF (IMINC.EQ.1) THEN                                           ! DLT
            HYC1 = MAX(HYC1,TINY)                                       ! DLT
         END IF                                                         ! DLT
         IF(HYC1.GT.ZERO) THEN
C3------CALCULATE VERTICAL HYDRAULIC CONDUCTIVITY FOR CELL BELOW.
            IF(LAYVKA(K+1).EQ.0) THEN
               HYC2=VKA(J,I,K+1)
            ELSE
               HYC2=(HK(J,I,K+1)/VKA(J,I,K+1))
            END IF
            IF (IMINC.EQ.1) THEN                                        ! DLT
               HYC2 = MAX(HYC2,TINY)                                    ! DLT
            END IF                                                      ! DLT
            IF(HYC2.GT.ZERO) THEN
C
C4------CALCULATE INVERSE LEAKANCE FOR CELL.  ICONCV FLAG PREVENTS
C4------CV FROM BEING HEAD DEPENDENT.
               BBOT=BOTM(J,I,LBOTM(K))
               TTOP=BOTM(J,I,LBOTM(K)-1)
               IF(LAYSTRT(K).NE.0) TTOP=STRT(J,I,K)
               IF(LAYTYP(K).NE.0 .AND. ICONCV.EQ.0) THEN
                  HHD=HNEW(J,I,K)
                  IF(HHD.LT.TTOP) TTOP=HHD
               END IF
               BOVK1=(TTOP-BBOT)*HALF/HYC1
C
C5------CALCULATE INVERSE LEAKANCE FOR CELL BELOW.
               BBOT=BOTM(J,I,LBOTM(K+1))
               TTOP=BOTM(J,I,LBOTM(K+1)-1)
               IF(LAYSTRT(K+1).NE.0) TTOP=STRT(J,I,K+1)
               B=(TTOP-BBOT)*HALF
C
C5A-----IF CELL BELOW IS NOT SATURATED, DO NOT INCLUDE ITS CONDUCTANCE
C5A-----IN THE VERTICAL CONDUCTANCE CALULATION, EXCEPT THAT THE NOCVCO
C5A-----AND ICONCV FLAGS TURN OFF THIS CORRECTION.
               IF(LAYTYP(K+1).NE.0
     1                    .AND.NOCVCO.EQ.0 .AND. ICONCV.EQ.0) THEN
                  HHD=HNEW(J,I,K+1)
                  IF(HHD.LT.TTOP) B=ZERO
               END IF
               BOVK2=B/HYC2
C
C6------CALCULATE VERTICAL HYDRAULIC CONDUCTIVITY FOR CONFINING BED.
               IF(LAYCBD(K).NE.0) THEN
                  HYC3 = VKCB(J,I,LAYCBD(K))                            ! DLT
                  IF (IMINC.EQ.1) THEN                                  ! DLT
                     HYC3 = MAX(HYC3,TINY)                              ! DLT
                  END IF                                                ! DLT
                  IF(HYC3.GT.ZERO) THEN                                 ! DLT
C
C7------CALCULATE INVERSE LEAKANCE FOR CONFINING BED.
                     B=BOTM(J,I,LBOTM(K))-BOTM(J,I,LBOTM(K)+1)
                     IF(B.LT.ZERO) THEN
                        WRITE(IOUT,45) K,I,J
   45                   FORMAT(1X,/1X,
     1  'Negative confining bed thickness below cell (Layer,row,col)',
     2                  I4,',',I5,',',I5)
            WRITE(IOUT,46) BOTM(J,I,LBOTM(K)),BOTM(J,I,LBOTM(K)+1)
   46       FORMAT(1X,'Top elevation, bottom elevation:',1P,2G13.5)
                        CALL USTOP(' ')
                     END IF
                     CBBOVK=B/HYC3
                     IF (HYC1.EQ.TINY) BOVK1  = 0.                      ! DLT
                     IF (HYC2.EQ.TINY) BOVK2  = 0.                      ! DLT
                     IF (HYC3.EQ.TINY) CBBOVK = 0.                      ! DLT
                     SUMVAL = BOVK1+CBBOVK+BOVK2
                     IF (IMINC.EQ.1) THEN                               ! DLT
                        SUMVAL = MIN(SUMVAL,MAXC)                       ! DLT
                        SUMVAL = MAX(SUMVAL,MINC)                       ! DLT
                     END IF                                             ! DLT
                     CV(J,I,K)=DELR(J)*DELC(I)/SUMVAL                   ! DLT
                  END IF
               ELSE
                  IF (HYC1.EQ.TINY) BOVK1  = 0.                         ! DLT
                  IF (HYC2.EQ.TINY) BOVK2  = 0.                         ! DLT
                  SUMVAL = BOVK1+BOVK2
                  IF (IMINC.EQ.1) THEN                                  ! DLT
                     SUMVAL = MIN(SUMVAL,MAXC)                          ! DLT
                     SUMVAL = MAX(SUMVAL,MINC)                          ! DLT
                  END IF                                                ! DLT
                  CV(J,I,K)=DELR(J)*DELC(I)/SUMVAL                      ! DLT
               END IF
            END IF
         END IF
      END IF
  100 CONTINUE
C
C8------RETURN.
      RETURN
      END
      SUBROUTINE SGWF2LPF7CK(IOUT,NP,PTYP)
C     ******************************************************************
C     CHECK THAT JUST-DEFINED PARAMETER OF TYPE 'VK' OR 'VANI' IS USED
C     CONSISTENTLY WITH LAYVKA ENTRIES FOR LAYERS LISTED IN CLUSTERS FOR
C     THE PARAMETER
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFLPFMODULE,  ONLY:LAYTYP,LAYAVG,CHANI,LAYVKA,LAYWET
      USE PARAMMODULE
C
      CHARACTER*4 PTYP
C     ------------------------------------------------------------------
C
C1------LOOP THROUGH THE CLUSTERS FOR THIS PARAMETER.
      DO 10 ICL = IPLOC(1,NP),IPLOC(2,NP)
        LAY = IPCLST(1,ICL)
        LV = LAYVKA(LAY)
        IF (PTYP.EQ.'VK  ' .AND. LV.NE.0) THEN
          WRITE (IOUT,590) LAY,LV,LAY,PARNAM(NP),'VK'
  590     FORMAT(/,
     &1X,'LAYVKA entered for layer ',i3,' is: ',i3,'; however,',
     &' layer ',i3,' is',/,' listed in a cluster for parameter "',a,
     &'" of type ',a,' and')
          WRITE (IOUT,600)
  600     FORMAT(
     &1X,'parameters of type VK can apply only to layers for which',
     &/,' LAYVKA is specified as zero -- STOP EXECUTION (SGWF2LPF7CK)')
          CALL USTOP(' ')
        ELSEIF (PTYP.EQ.'VANI' .AND. LV.EQ.0) THEN
          WRITE (IOUT,590) LAY,LV,LAY,PARNAM(NP),'VANI'
          WRITE (IOUT,610)
  610     FORMAT(
     &1X,'parameters of type VANI can apply only to layers for which',/,
     &' LAYVKA is not specified as zero -- STOP EXECUTION',
     &' (SGWF2LPF7CK)')
          CALL USTOP(' ')
        ENDIF
   10 CONTINUE
C
C2------Return.
      RETURN
      END
      SUBROUTINE GWF2LPF7DA(IGRID)
C  Deallocate LPF DATA
      USE GWFLPFMODULE
C
        DEALLOCATE(GWFLPFDAT(IGRID)%ILPFCB)
        DEALLOCATE(GWFLPFDAT(IGRID)%IWDFLG)
        DEALLOCATE(GWFLPFDAT(IGRID)%IWETIT)
        DEALLOCATE(GWFLPFDAT(IGRID)%IHDWET)
        DEALLOCATE(GWFLPFDAT(IGRID)%ISFAC)
        DEALLOCATE(GWFLPFDAT(IGRID)%ICONCV)
        DEALLOCATE(GWFLPFDAT(IGRID)%ITHFLG)
        DEALLOCATE(GWFLPFDAT(IGRID)%NOCVCO)
        DEALLOCATE(GWFLPFDAT(IGRID)%NOVFC)
        DEALLOCATE(GWFLPFDAT(IGRID)%WETFCT)
        DEALLOCATE(GWFLPFDAT(IGRID)%LAYTYP)
        DEALLOCATE(GWFLPFDAT(IGRID)%LAYAVG)
        DEALLOCATE(GWFLPFDAT(IGRID)%CHANI)
        DEALLOCATE(GWFLPFDAT(IGRID)%LAYVKA)
        DEALLOCATE(GWFLPFDAT(IGRID)%LAYWET)
        DEALLOCATE(GWFLPFDAT(IGRID)%LAYSTRT)
        DEALLOCATE(GWFLPFDAT(IGRID)%LAYFLG)
        DEALLOCATE(GWFLPFDAT(IGRID)%VKA)
        DEALLOCATE(GWFLPFDAT(IGRID)%VKCB)
        DEALLOCATE(GWFLPFDAT(IGRID)%SC1)
        DEALLOCATE(GWFLPFDAT(IGRID)%SC2)
        DEALLOCATE(GWFLPFDAT(IGRID)%HANI)
        DEALLOCATE(GWFLPFDAT(IGRID)%WETDRY)
        DEALLOCATE(GWFLPFDAT(IGRID)%HK)
        DEALLOCATE(GWFLPFDAT(IGRID)%IMINKD)                             ! DLT
        DEALLOCATE(GWFLPFDAT(IGRID)%IMINC)                              ! DLT
        DEALLOCATE(GWFLPFDAT(IGRID)%MINKD)                              ! DLT
        DEALLOCATE(GWFLPFDAT(IGRID)%MINC)                               ! DLT
C
      RETURN
      END
      SUBROUTINE SGWF2LPF7PNT(IGRID)
C  Point to LPF data for a grid.
      USE GWFLPFMODULE
C
        ILPFCB=>GWFLPFDAT(IGRID)%ILPFCB
        IWDFLG=>GWFLPFDAT(IGRID)%IWDFLG
        IWETIT=>GWFLPFDAT(IGRID)%IWETIT
        IHDWET=>GWFLPFDAT(IGRID)%IHDWET
        ISFAC=>GWFLPFDAT(IGRID)%ISFAC
        ICONCV=>GWFLPFDAT(IGRID)%ICONCV
        ITHFLG=>GWFLPFDAT(IGRID)%ITHFLG
        NOCVCO=>GWFLPFDAT(IGRID)%NOCVCO
        NOVFC=>GWFLPFDAT(IGRID)%NOVFC
        WETFCT=>GWFLPFDAT(IGRID)%WETFCT
        LAYTYP=>GWFLPFDAT(IGRID)%LAYTYP
        LAYAVG=>GWFLPFDAT(IGRID)%LAYAVG
        CHANI=>GWFLPFDAT(IGRID)%CHANI
        LAYVKA=>GWFLPFDAT(IGRID)%LAYVKA
        LAYWET=>GWFLPFDAT(IGRID)%LAYWET
        LAYSTRT=>GWFLPFDAT(IGRID)%LAYSTRT
        LAYFLG=>GWFLPFDAT(IGRID)%LAYFLG
        VKA=>GWFLPFDAT(IGRID)%VKA
        VKCB=>GWFLPFDAT(IGRID)%VKCB
        SC1=>GWFLPFDAT(IGRID)%SC1
        SC2=>GWFLPFDAT(IGRID)%SC2
        HANI=>GWFLPFDAT(IGRID)%HANI
        WETDRY=>GWFLPFDAT(IGRID)%WETDRY
        HK=>GWFLPFDAT(IGRID)%HK
        IMINKD=>GWFLPFDAT(IGRID)%IMINKD                                 ! DLT
        IMINC=>GWFLPFDAT(IGRID)%IMINC                                   ! DLT
        MINKD=>GWFLPFDAT(IGRID)%MINKD                                   ! DLT
        MINC=>GWFLPFDAT(IGRID)%MINC                                     ! DLT
C
      RETURN
      END
      SUBROUTINE GWF2LPF7PSV(IGRID)
C  Save LPF data for a grid.
      USE GWFLPFMODULE
C
        GWFLPFDAT(IGRID)%ILPFCB=>ILPFCB
        GWFLPFDAT(IGRID)%IWDFLG=>IWDFLG
        GWFLPFDAT(IGRID)%IWETIT=>IWETIT
        GWFLPFDAT(IGRID)%IHDWET=>IHDWET
        GWFLPFDAT(IGRID)%ISFAC=>ISFAC
        GWFLPFDAT(IGRID)%ICONCV=>ICONCV
        GWFLPFDAT(IGRID)%ITHFLG=>ITHFLG
        GWFLPFDAT(IGRID)%NOCVCO=>NOCVCO
        GWFLPFDAT(IGRID)%NOVFC=>NOVFC
        GWFLPFDAT(IGRID)%WETFCT=>WETFCT
        GWFLPFDAT(IGRID)%LAYTYP=>LAYTYP
        GWFLPFDAT(IGRID)%LAYAVG=>LAYAVG
        GWFLPFDAT(IGRID)%CHANI=>CHANI
        GWFLPFDAT(IGRID)%LAYVKA=>LAYVKA
        GWFLPFDAT(IGRID)%LAYWET=>LAYWET
        GWFLPFDAT(IGRID)%LAYSTRT=>LAYSTRT
        GWFLPFDAT(IGRID)%LAYFLG=>LAYFLG
        GWFLPFDAT(IGRID)%VKA=>VKA
        GWFLPFDAT(IGRID)%VKCB=>VKCB
        GWFLPFDAT(IGRID)%SC1=>SC1
        GWFLPFDAT(IGRID)%SC2=>SC2
        GWFLPFDAT(IGRID)%HANI=>HANI
        GWFLPFDAT(IGRID)%WETDRY=>WETDRY
        GWFLPFDAT(IGRID)%HK=>H K
        GWFLPFDAT(IGRID)%IMINKD=>IMINKD                                 ! DLT
        GWFLPFDAT(IGRID)%IMINC=>IMINC                                   ! DLT
        GWFLPFDAT(IGRID)%MINKD=>MINKD                                   ! DLT
        GWFLPFDAT(IGRID)%MINC=>MINC                                     ! DLT
C
      RETURN
      END
