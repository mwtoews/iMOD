
MODULE MOD_WBAL_GRAPHICS
!     Andre Blonk Tauw Deventer
!     Augustus 2017
!     programma voor de visuallisatie van een waterbalans (een zogenaamde illustratieve plot)

USE WINTERACTER
USE MOD_IDF_PAR, ONLY : IDFOBJ

CONTAINS
!###====================================================================
      LOGICAL FUNCTION DRAWBAL(Q,QTXT,NXPIX,NYPIX,CS,IIPOL,NPOL,QSUBREGIO,PERC,ICOLPLG,IPLG,IDF,LOCAL,GRAPHTITLE,IPLOT,LSUM,STRUNIT,SUMNR)    
!###====================================================================
!     THIS LOGICAL FUNCTION GENERATES A PNG FILE CONTAINING A PICTURE OF AN AVERAGE WATERBALANCE
!     THE FOLLOWING TERMS ARE USED
!     Q(01,1)=QDRN_IN   Q(01,2)=QDRN_OUT    Q(13,1)=QCAP_IN     Q(13,1)=QCAP_OUT    
!     Q(02,1)=QOLF_IN   Q(02,2)=QOLF_OUT    Q(14,1)=QETACT_IN   Q(14,1)=QETACT_OUT  
!     Q(03,1)=QRIV_IN   Q(03,2)=QRIV_OUT    Q(15,1)=QPM_IN      Q(15,1)=QPM_OUT     
!     Q(04,1)=QGHB_IN   Q(04,2)=QGHB_OUT    Q(16,1)=QPMGW_IN    Q(16,1)=QPMGW_OUT   
!     Q(05,1)=QISG_IN   Q(05,2)=QISG_OUT    Q(17,1)=QPMSW_IN    Q(17,1)=QPMSW_OUT   
!     Q(06,1)=QWEL_IN   Q(06,2)=QWEL_OUT    Q(18,1)=QSTO_IN     Q(18,1)=QSTO_OUT    
!     Q(07,1)=QREG_IN   Q(07,2)=QREG_OUT    Q(19,1)=QDECSTO_IN  Q(19,1)=QDECSTO_OUT 
!     Q(08,1)=QCNH_IN   Q(08,2)=QCNH_OUT    Q(20,1)=QQSPGW_IN   Q(20,1)=QQSPGW_OUT  
!     Q(09,1)=QFLF1_IN  Q(09,2)=QFLF1_OUT   Q(21,1)=QQCOR_IN    Q(21,1)=QQCOR_OUT   
!     Q(10,1)=QFLF2_IN  Q(10,2)=QFLF2_OUT   Q(22,1)=QQDR_IN     Q(22,1)=QQDR_OUT    
!     Q(11,1)=QRCH_IN   Q(11,2)=QRCH_OUT    Q(23,1)=QQRUN_IN    Q(23,1)=QQRUN_OUT   
!     Q(12,1)=QEVT_IN   Q(12,2)=QEVT_OUT    Q(24,1)=QMODF_IN    Q(24,1)=QMODF_OUT   

      IMPLICIT NONE
      INTEGER,DIMENSION(NPOL), INTENT(IN) ::  ICOLPLG,IPLG
      REAL,DIMENSION(NPOL,2),INTENT(INOUT) :: QSUBREGIO
      REAL,DIMENSION(24,2),INTENT(INOUT) :: Q
      CHARACTER(LEN=*),DIMENSION(24),INTENT(INOUT) :: QTXT
      CHARACTER(LEN=*),INTENT(IN) :: GRAPHTITLE
      INTEGER, INTENT(IN) :: NPOL,SUMNR
      INTEGER, INTENT(IN),DIMENSION(:) :: IIPOL
      REAL,DIMENSION(4) :: XR,YR
      CHARACTER(LEN=256) :: STR,REGNAME
      CHARACTER(LEN=10) :: STRDUM
      CHARACTER(LEN=8) :: FMT
      INTEGER :: NXPIX,NYPIX,IPLOT,IUNSATURATED,ISATURATED,IBL1,IGR1,IRD1,IBL2,IGR2,IRD2,&
                 II,JJ,JPOL,I,J,K,IW,IWMAX,JW,JWMAX,IPOL,JDUM,IPOS,IFIT,JFIT,IXPIX,IYPIX
      REAL :: XPOS1,XPOS2,YPOS,XPIX,YPIX
      REAL :: SUMIN,SUMOUT,AREA,CS,B,H
      REAL :: DW,DH,DY,DX
      LOGICAL :: LOCAL,LSUM,PERC
      CHARACTER(LEN=*),INTENT(IN) :: STRUNIT
       
      TYPE (IDFOBJ),INTENT(IN) :: IDF


      DRAWBAL=.FALSE.
      B=10000
      H=10000
!     VERTICAL SHIFT GRAPH
      DX=0.05
      DY=0.05
      SUMIN  =SUM(Q(1:24,1))
      SUMOUT =SUM(Q(1:24,2))
      IF(INDEX(STRUNIT,'%').NE.0) THEN
!        CALCULATE RELATIVE FLOWS IN %
         Q(:,1)=Q(:,1)/SUMIN*100      	
         Q(:,2)=Q(:,2)/SUMIN*100      	
         QSUBREGIO(:,1)=QSUBREGIO(:,1)/SUMIN*100
         QSUBREGIO(:,2)=QSUBREGIO(:,2)/SUMIN*100
         SUMOUT=SUMOUT/SUMIN*100
         SUMIN=100
      ENDIF
     
      CALL IGRCOLOURMODEL(24)
      CALL WBITMAPCREATE(IPLOT,NXPIX,NYPIX)                                                                      
      CALL IGRSELECT(DRAWBITMAP,IPLOT)       
      CALL IGRAREA(0.0,0.5+DY,1.0,0.95+DY)
      CALL IGRUNITS(0.0,0.0,B,1.1*H)
      CALL IGRFILLPATTERN(4,4,4)
      CALL IGRCOLOURN(WRGB(255,255,100))

!     check saturated unsaturated zone or both
      IUNSATURATED=0; IF(SUM(Q(14:24,1)).NE.0.OR.SUM(Q(14:24,2)).NE.0) IUNSATURATED=1
      ISATURATED  =0; IF(SUM(Q(1 :13,1)).NE.0.OR.SUM(Q(1 :13,2)).NE.0) ISATURATED=1
!     plotting of unsaturated/satuarted zone is always
      IUNSATURATED=1
      ISATURATED  =1

      IF(IUNSATURATED.NE.0) THEN
         CALL IGRRECTANGLE((0.2-dx)*B,0.2*H,(0.8+dx)*B,0.8*H)
         CALL IGRFILLPATTERN(0,0,0)
         CALL PLOTTREE(0.34*B,0.79*H,0.15*H,1.1)
         CALL PLOTTREE(0.42*B,0.79*H,0.13*H,1.1)
      ENDIF
      CALL IGRAREA(0.2-dx,0.48+dy ,0.8+dx,0.7+dy)
      CALL IGRUNITS(0.0,0.0,B,H)
!     Unsaturated zone with gradient fill
      IF(IUNSATURATED.EQ.1) THEN
         IRD1=190; IGR1=215;  IBL1=240
         IRD2=255; IGR2=255;  IBL2=100
!        unsaturated zone from yellow to blue.....
         CALL IGRCOLOURN(WRGB(IRD1,IGR1,IBL1))
         CALL IGRCOLOURN(WRGB(IRD2,IGR2,IBL2))
         XR(1)=0.0; YR(1)=0.0
         XR(2)=B; YR(2)=YR(1)
         XR(3)=B; YR(3)=H
         XR(4)=0; YR(4)=YR(3)
         CALL IGRPOLYGONGRAD(XR,YR,4,1)
      ENDIF

      CALL IGRAREA(0.0,0.0+dy,1.0,0.7+dy)
      CALL IGRUNITS(0.0,0.0,B,1.1*H)

!     PLOT separating layers
      IF(ISATURATED.EQ.1) THEN
         CALL IGRFILLPATTERN(4,4,4)
         CALL IGRCOLOURN(WRGB(190,215,240))
         CALL IGRRECTANGLE((0.2-dx)*B,0.18*H,(0.8+dx)*B,0.82*H)
         CALL IGRFILLPATTERN(0,0,0)
         CALL IGRLINEWIDTH(3)
         IF(Q(9,1).NE.0.0.OR.Q(9,2).NE.0.0) THEN
            CALL IGRFILLPATTERN(4,4,4)
            CALL IGRCOLOURN(WRGB(195,225,180))
            CALL IGRRECTANGLE((0.2-dx)*B,0.78*H,(0.8+dx)*B,0.82*H)
            CALL IGRFILLPATTERN(0,0,0)
            CALL IGRCOLOURN(WRGB(  0,155,  0))
            CALL IGRRECTANGLE((0.2-dx)*B,0.78*H,(0.8+dx)*B,0.82*H)
         ENDIF
         IF(Q(10,1).NE.0.0.OR.Q(10,2).NE.0.0) THEN
            CALL IGRFILLPATTERN(4,4,4)
            CALL IGRCOLOURN(WRGB(195,225,180))
            CALL IGRRECTANGLE((0.2-dx)*B,0.18*H,(0.8+dx)*B,0.22*H)
            CALL IGRFILLPATTERN(0,0,0)
            CALL IGRCOLOURN(WRGB(  0,155,  0))
            CALL IGRRECTANGLE((0.2-dx)*B,0.18*H,(0.8+dx)*B,0.22*H)
         ENDIF
      ENDIF
      CALL CSIZE(CS,DW,DH,IPLOT)
      CALL WGRTEXTFONT(102,1,DW,DH)
      CALL PLOTDRAIN(Q(1,1),Q(1,2),0.22,0.75,0.015,QTXT(1),B,H)                                ! PLOT DRN+OLF FLOW
      CALL PLOTDRAIN(Q(2,1),Q(2,2),0.19,0.75,0.015,QTXT(2),B,H)                                ! PLOT DRN+OLF FLOW

      CALL PLOTRIV(Q(3,1),Q(3,2),0.27,WRGB(100,100,255),QTXT(3),B,H)                           ! PLOT RIV FLOW
      CALL PLOTRIV(Q(5,1),Q(5,2),0.34,WRGB(150,150,255),QTXT(5),B,H)                           ! PLOT ISG FLOW
      CALL PLOTRIV(Q(4,1),Q(4,2),0.41,WRGB(100,255,100),QTXT(4),B,H)                           ! PLOT GHB FLOW

      CALL PLOTWEL(Q(6,1),Q(6,2),0.62,QTXT(6),B,H)                                             ! PLOT WELL FLOW
      CALL PLOTREG(Q(7,1),Q(7,2),0.66,QTXT(7),B,H)                                             ! PLOT REGIO FLOW
      CALL PLOTREG(Q(8,1),Q(8,2),0.25,QTXT(8),B,H)                                             ! PLOT CONSTANT HEAD FLOW

!     regional flows from npol other regions to chosen ipol region and
!     regional flows from chosen ipol region to npol other regions

!     determine the name of the single or summed regio(s)
      II=0; REGNAME=' '
      IF(LSUM) THEN
         DO J=1,SIZE(IIPOL)
            WRITE(STRDUM(1:7),'(I7)') IPLG(IIPOL(J))
            DO JJ=1,7
               IF(STRDUM(JJ:JJ).EQ.' ') IPOS=JJ+1
            ENDDO
            WRITE(REGNAME(II+1:II+7-IPOS+2),'(A,A))') STRDUM(IPOS:7),','
            II=II+7-IPOS+2
         ENDDO
         II=LEN_TRIM(REGNAME); REGNAME(II:II)=' '
      ELSE
         WRITE(STRDUM(1:7),'(I7)') SUMNR
         DO JJ=1,7
            IF(STRDUM(JJ:JJ).EQ.' ') IPOS=JJ+1
         ENDDO
         WRITE(REGNAME(II+1:II+7-IPOS+2),'(A,A))') STRDUM(IPOS:7),','
         II=LEN_TRIM(REGNAME); REGNAME(II:II)=' '
      ENDIF


      II=0; JJ=0
      CALL CSIZE(CS,DW,DH,IPLOT)
      DH=1.2*DH*H
      XPOS1=0; XPOS2=0
      JDUM=0
      DO JPOL=1,NPOL
        DO J=1,SIZE(IIPOL)
         IPOL=IIPOL(J)
         IF(JPOL.NE.IPOL.AND.JPOL.NE.JDUM) THEN
!          determine interregional fluxes, these are fluxe between interconnecting summed regions
           IFIT=0
           IF(LSUM) THEN                                           
              DO JFIT=1,SIZE(IIPOL)                             
                 IF(IPLG(JPOL).EQ.IPLG(IIPOL(JFIT))) IFIT=1    ! INTERREGIONALE FLUXEN
              ENDDO        
           ELSE
              DO JFIT=1,SIZE(IIPOL)                             
                 IF(IPLG(JPOL).EQ.SUMNR) IFIT=1                
              ENDDO        
           ENDIF                                     

!          plotting subregio text.....
           CALL PLOTSUBREGIO(II,JJ,-QSUBREGIO(JPOL,2),-QSUBREGIO(JPOL,1),REGNAME,IPOL,JPOL,NPOL,ICOLPLG,IPLG,DH,B,H,IFIT)

!          testing.....calculate width of plotted text starting at position x=0.13*b, y=0.65*h-(ii-0.5)*dh, until the centre....nxpix/2
!          left side of plot, calculating width of box
           CALL IGRUNITSTOPIXELS(0.13*B,0.65*H-(II-0.5)*DH,IXPIX,IYPIX)
           DO I=IXPIX,NXPIX/2
              CALL IGRUNITSFROMPIXELS(I,IYPIX,XPIX,YPIX)
              IF(IGRGETPIXEL(XPIX,YPIX).EQ.WRGB(0,0,0)) THEN
              	 YPOS=YPIX; if(xpix.gt.xpos1) xpos1=xpix
              ENDIF
           ENDDO
!          right side of plot, calculating width of box
           CALL IGRUNITSTOPIXELS(0.76*B,0.65*H-(II-0.5)*DH,IXPIX,IYPIX)
           DO I=IXPIX,NXPIX
              CALL IGRUNITSFROMPIXELS(I,IYPIX,XPIX,YPIX)
              IF(IGRGETPIXEL(XPIX,YPIX).EQ.WRGB(0,0,0)) THEN
              	 YPOS=YPIX; if(xpix.gt.xpos2) xpos2=xpix
              ENDIF
           ENDDO

           JDUM=JPOL
        ENDIF
        ENDDO
      ENDDO
      CALL DVOLUME(Q(18,1),Q(18,2),0.70,QTXT(18),B,H)                        ! PLOT STORAGE FLOW


      CALL IGRLINETYPE(2)
      CALL IGRCOLOURN(WRGB(0,0,0))
      CALL IGRFILLPATTERN(0,0,0)
!     PLOT DASHED RECTANGLE AROUND SUBREGIOFLOW
      IF(II.GT.0) CALL IGRRECTANGLE(0.13*B,0.65*H,xpos1+0.01*B,0.65*H-(II+0.5)*DH)
      IF(JJ.GT.0) CALL IGRRECTANGLE(0.76*B,0.65*H,xpos2+0.01*B,0.65*H-(JJ+0.5)*DH)

      CALL IGRLINETYPE(0)
      CALL VTERM2(Q(11,1),Q(11,2),0.56, 0.58, 0.8,   QTXT(11),B,H,2,1)       ! PLOT RCH FLOW
      CALL VTERM2(Q(12,1),Q(12,2),0.62, 0.64, 0.8,   QTXT(12),B,H,2,1)       ! PLOT EVT FLOW
      CALL VTERM2(Q(13,1),Q(13,2),0.68, 0.70, 0.8,   QTXT(13),B,H,2,1)       ! PLOT CAP FLOW
      CALL VTERM2(Q(10,1), Q(10,2), 0.74, 0.76, 0.8, QTXT(9), B,H,2,1)       ! PLOT FLF FLOW (FLOW TOP FACE)
      CALL VTERM2(Q(9,1),Q(9,2),0.49, 0.51, 0.22,     QTXT(10),B,H,3,0)       ! PLOT FLF FLOW (FLOW LOWER FACE)

!     *********************UNSATURATED*********************
      CALL IGRAREA(0.0,0.50+dy,1.0,0.95+dy)
      CALL IGRUNITS(0.0,0.0,B,1.1*H)
      CALL IGRFILLPATTERN(0,0,0)
 
      CALL CSIZE(CS,DW,DH,IPLOT)
      CALL WGRTEXTFONT(102,1,DW,DH)
      CALL VTERM2(Q(14,1),Q(14,2),0.50,0.52,0.8,QTXT(14),B,H,2,1)            ! PLOT METASWAP ETACT       
      CALL VTERM2(Q(15,1),Q(15,2),0.56,0.58,0.8,QTXT(15),B,H,2,1)            ! PLOT METASWAP PM
      CALL VTERM2(Q(23,1),Q(23,2),0.62,0.64,0.8,QTXT(23),B,H,2,1)            ! PLOT METASWAP QRUN  
      CALL VTERM2(Q(17,1),Q(17,2),0.68,0.70,0.8,QTXT(17),B,H,2,1)            ! PLOT METASWPP PMSW  
      CALL VTERM2(Q(16,1),Q(16,2),0.74,0.76,0.8,QTXT(16),B,H,2,1)            ! PLOT METASWPP PMGW  
                                                                                           
      CALL DVOLUME(Q(19,1),Q(19,2),   0.47,          QTXT(19),B,H)           ! PLOT METASWAP STORAGE
      CALL DVOLUME(Q(21,1),Q(21,1),   0.42,          QTXT(21),B,H)           ! PLOT METASWAP QCOR
      CALL PLOTDRAIN(Q(22,1),Q(22,2), 0.22,0.50,0.01,QTXT(22),B,H)           ! PLOT METASWAP QDR
      CALL PLOTDRAIN(Q(20,1),Q(20,2), 0.25,0.50,0.01,QTXT(20),B,H)           ! PLOT METASWPA QSPGW
      CALL VTERM2(Q(24,1),Q(24,2),0.48,0.50,0.00,    QTXT(24),B,H,0,0)       ! PLOT METASWAP QMODF

      CALL PLOTIDF(IIPOL,NPOL,IPLG,ICOLPLG,IPLOT,LOCAL,AREA,IDF,LSUM,SUMNR)

      CALL IGRAREA(0.0,0.0,1.0,1.0)
      CALL IGRUNITS(0.0,0.0,1.0,1.0)
      CALL WGRTEXTORIENTATION(0, 0.0,0)
      
      CALL IGRCOLOURN(WRGB(0,0,0))
      CALL CSIZE(CS,DW,DH,IPLOT)
      CALL WGRTEXTFONT(101,1,DW,DH)
     
      CALL WGRTEXTSTRING(0.15,0.03+4.5*DH,'WATERBALANCE REGION    '//TRIM(REGNAME)) 
      CALL OUTVALUE(0.15,0.03+3.5*DH,AREA/10000.0,          'AREA             HA   ')
      CALL OUTVALUE(0.15,0.03+2.5*DH,SUMIN ,        'TOTAL SUM IN     '//STRUNIT)
      CALL OUTVALUE(0.15,0.03+1.5*DH,SUMOUT,        'TOTAL SUM OUT    '//STRUNIT)
      CALL OUTVALUE(0.15,0.03+0.5*DH,SUMOUT+SUMIN,  'TOTAL SUM OUT+IN '//STRUNIT)
      CALL CSIZE(1.5*CS,DW,DH,IPLOT)
      CALL WGRTEXTFONT(102,1,DW,DH)
      CALL WGRTEXTORIENTATION(1,0.0,0)
      !CALL WGRTEXTSTRING(0.5,1.0-0.7*DH,GRAPHTITLE(1:LEN_TRIM(GRAPHTITLE)))
      CALL WGRTEXTBLOCK(0.15,0.95,0.85,1.0,GRAPHTITLE(1:LEN_TRIM(GRAPHTITLE)),1.0,4)
!     DRAW FRAMEWORK
      CALL IGRLINEWIDTH(2)
      CALL IGRRECTANGLE(0.0,0.0,1.0,1.0)
      CALL IGRLINEWIDTH(1)
      !CALL IGRSAVEIMAGE(FPNG)
      !IF(INFOERROR(3).NE.0) THEN
      !	 PAUSE 'ERROR OCCURRED WRITING PNG-FILE'
      !ENDIF
      !CALL WBITMAPDESTROY(IPLOT)
      DRAWBAL=.TRUE.

!      WRITE(STR(1:3),'(I3.3)') SUMNR
!      CALL IGRSAVEIMAGE('\ILLUSTRATIEVE_PLOT'//STR(1:3)//'.PNG')

      END FUNCTION DRAWBAL

!###====================================================================
      SUBROUTINE ARROWTEXT(X0,Y0,X1,Y1,XTXT,YTXT,IJUSTIFY,ANGLE,ICOLOR,Q,QTXT)
!###====================================================================
      IMPLICIT NONE
      REAL,INTENT(IN) :: X0,Y0,X1,Y1,XTXT,YTXT,ANGLE,Q
      INTEGER,INTENT(IN) :: IJUSTIFY,ICOLOR
      CHARACTER(LEN=*),INTENT(INOUT) :: QTXT
      
      CALL IGRFILLPATTERN(4,4,4)
      CALL IGRCOLOURN(ICOLOR)
      CALL IGRARROWJOIN(X0,Y0,X1,Y1,2)
      CALL WGRTEXTORIENTATION(IJUSTIFY,ANGLE,0)
      CALL OUTVALUE(XTXT,YTXT,Q,'  '//QTXT)
      
      END SUBROUTINE ARROWTEXT
      
!###====================================================================
      SUBROUTINE PLOTDRAIN(QIN,QUIT,FX,FY,P,QTXT,B,H)
!###====================================================================
      IMPLICIT NONE
      CHARACTER(LEN=*),INTENT(INOUT) :: QTXT
      REAL,INTENT(IN) :: B,H,FX,FY,P,QIN,QUIT
      REAL :: XDRN,YDRN
      INTEGER :: IPLOT,ILEN
      
      CALL IGRLINEWIDTH(1)
      CALL IGRFILLPATTERN(0,0,0)
      XDRN=FX*B
      YDRN=FY*H
      IF(QUIT+QIN.LT.0) THEN
         CALL IGRFILLPATTERN(4,4,4)
         CALL IGRCOLOURN(WRGB(255,100,100))
         CALL IGRCIRCLE(XDRN,YDRN,P*B)
         CALL IGRFILLPATTERN(0,0,0)
         CALL IGRCOLOURN(WRGB(0,0,0))
         CALL IGRCIRCLE(XDRN,YDRN,P*B)
         CALL ARROWTEXT(XDRN,YDRN,XDRN,YDRN+0.1*h,&
                        XDRN,YDRN+0.1*H,&
                        0,90.0,WRGB(255,0,0),ABS(QIN+QUIT),QTXT(1:LEN_TRIM(QTXT)))
      ENDIF
      IF(QIN+QUIT.GT.0) THEN
         CALL IGRFILLPATTERN(4,4,4)
         CALL IGRCOLOURN(WRGB(255,100,100))
         CALL IGRCIRCLE(XDRN,YDRN,P*B)
         CALL IGRFILLPATTERN(0,0,0)
         CALL IGRCOLOURN(WRGB(0,0,0))
         CALL IGRCIRCLE(XDRN,YDRN,P*B)
         CALL ARROWTEXT(XDRN,YDRN+0.1*H,XDRN,YDRN,&
                        XDRN,YDRN+0.1*H,&
                        0,90.0,WRGB(0,0,255),ABS(QIN+QUIT),QTXT(1:LEN_TRIM(QTXT)))
      ENDIF                                     
      
      END SUBROUTINE PLOTDRAIN

!###====================================================================
      SUBROUTINE DVOLUME(QIN,QUIT,F,QTXT,B,H)
!###====================================================================
      IMPLICIT NONE
      REAL,INTENT(IN) :: QIN,QUIT,B,H,F
      CHARACTER(LEN=*),INTENT(INOUT) :: QTXT
      REAL :: X,Y
      INTEGER :: IPLOT,ILEN
      
      IF(QIN.NE.0.OR.QUIT.NE.0) THEN
      	 X=0.33*B
      	 Y=F*H
         CALL IGRFILLPATTERN(4,4,4)
         IF(QIN+QUIT.LT.0) THEN
             CALL IGRCOLOURN(WRGB(255,0,0))
         ELSE
             CALL IGRCOLOURN(WRGB(0,0,255))
         ENDIF
         CALL IGRELLIPSE(X,Y,B/100,1.4)
         CALL IGRMOVETO(X,Y)
         CALL IGRLINETO(X+0.1*B,Y)
         CALL IGRCOLOURN(WRGB(0,0,0))
         CALL WGRTEXTORIENTATION(0,0.0,0)
         CALL OUTVALUE(X,Y+0.025*H,ABS(QIN+QUIT),QTXT(1:LEN_TRIM(QTXT)))
      ENDIF
      
      END SUBROUTINE DVOLUME

!###====================================================================
      SUBROUTINE PLOTRIV(QIN,QUIT,F,ICOL,QTXT,B,H)                                                                     
!###====================================================================
      IMPLICIT NONE
      REAL,INTENT(IN) :: QIN,QUIT,B,H,F
      INTEGER,INTENT(IN) :: ICOL
      CHARACTER(LEN=*),INTENT(INOUT) :: QTXT                                                                                    
      REAL :: XTRAP1,YTRAP1,XTRAP2,YTRAP2,XL1,XL2
      INTEGER :: IPLOT

      IF(QIN.NE.0.OR.QUIT.NE.0) THEN                                                                        
!        PLOT RIVER                                                                                         
         XTRAP1=F*B; YTRAP1=0.75*H                                                                                       
         XL1=0.025*B; XL2=3*XL1
         XTRAP2=XTRAP1+2*XL1
         YTRAP2=0.8*H                                                                                       
         CALL IGRFILLPATTERN(4,4,4)                                                                         
         CALL IGRCOLOURN(ICOL)                                                                              
         CALL IGRTRAPEZIUM(XTRAP1,YTRAP1,XTRAP2,YTRAP2,XL1,XL2,2)                                           
         CALL IGRFILLPATTERN(0,0,0)                                                                         
         CALL IGRCOLOURN(WRGB(0,0,0))                                                                       
         CALL IGRTRAPEZIUM(XTRAP1,YTRAP1,XTRAP2,YTRAP2,XL1,XL2,2)                                           
         IF(QUIT.NE.0) THEN                                                                                 
            CALL ARROWTEXT(XTRAP1,(YTRAP2+YTRAP1)/2,XTRAP1,(YTRAP2+YTRAP1)/2+0.1*H,&
                           XTRAP1,(YTRAP2+YTRAP1)/2+0.1*H,&
                           0,90.0,WRGB(255,0,0),ABS(QUIT),QTXT(1:LEN_TRIM(QTXT)))
         ENDIF                                                                                              
         IF(QIN.NE.0) THEN                                                                                  
            CALL ARROWTEXT(XTRAP1+XL1,(YTRAP2+YTRAP1)/2+0.1*H,XTRAP1+XL1,(YTRAP2+YTRAP1)/2,&
                           XTRAP1+XL1,(YTRAP2+YTRAP1)/2+0.1*H,&
                           0,90.0,WRGB(0,0,255),QIN,QTXT(1:LEN_TRIM(QTXT)))
         ENDIF                                                                                              
      ENDIF                                                                                                 

      END SUBROUTINE PLOTRIV
      
!###====================================================================
      SUBROUTINE PLOTWEL(QIN,QUIT,F,QTXT,B,H)
!###====================================================================
      IMPLICIT NONE
      REAL,INTENT(IN) :: QIN,QUIT,B,H,F
      CHARACTER(LEN=*),INTENT(INOUT) :: QTXT
      REAL :: XWEL
      INTEGER :: IPLOT

      IF(QIN.NE.0.OR.QUIT.NE.0) THEN
!        PLOT WEL
         XWEL=F*B
         CALL IGRFILLPATTERN(4,4,4)
         CALL IGRCOLOURN(WRGB(255,100,255))
         CALL IGRRECTANGLE(XWEL-0.01*B,0.3*H,XWEL+0.01*B,0.6*H)
         CALL IGRFILLPATTERN(0,0,0)
         CALL IGRCOLOURN(WRGB(0,0,0))
         CALL IGRRECTANGLE(XWEL-0.01*B,0.3*H,XWEL+0.01*B,0.6*H)
         CALL IGRRECTANGLE(XWEL-0.006*B,0.3*H,XWEL+0.006*B,0.6*H)
         CALL IGRRECTANGLE(XWEL-0.002*B,0.3*H,XWEL+0.002*B,0.6*H)
         IF(QUIT.NE.0) THEN
            CALL ARROWTEXT(XWEL-0.03*B,0.5*H,XWEL-0.03*B,0.6*H,&
                           XWEL-0.03*B,0.5*H,&
                           2,90.0,WRGB(255,0,0),ABS(QUIT),QTXT(1:LEN_TRIM(QTXT)))
         ENDIF
         IF(QIN.NE.0) THEN
            CALL ARROWTEXT(XWEL+0.03*B,0.6*H,XWEL+0.03*B,0.5*H,&
                           XWEL+0.03*B,0.5*H,&
                           2,90.0,WRGB(0,0,255),QIN,QTXT(1:LEN_TRIM(QTXT)))
         ENDIF
      ENDIF
      
      END SUBROUTINE PLOTWEL

!###====================================================================
      SUBROUTINE PLOTREG(QIN,QUIT,F,QTXT,B,H)
!###====================================================================
      IMPLICIT NONE
      REAL,INTENT(IN) :: QIN,QUIT,B,H,F
      CHARACTER(LEN=*),INTENT(INOUT) :: QTXT
      INTEGER :: IPLOT
      
      IF(QIN.NE.0)  CALL ARROWTEXT(0.1*B,F*H,0.2*B,F*H,&
                                   0.1*B,(F+0.025)*H,&
                                   0,0.0,WRGB(0,0,255),QIN,QTXT(1:LEN_TRIM(QTXT)))

      IF(QUIT.NE.0) CALL ARROWTEXT(0.8*B,F*H,0.9*B,F*H,&
                                   0.8*B,(F+0.025)*H,&
                                   0,0.0,WRGB(255,0,0),ABS(QUIT),QTXT(1:LEN_TRIM(QTXT)))

      END SUBROUTINE PLOTREG

!###====================================================================
      SUBROUTINE VTERM2(QIN,QUIT,FX1,FX2,FY,QTXT,B,H,IPOS,ITOP)             
!###====================================================================
      IMPLICIT NONE                                              
      REAL,INTENT(IN) :: QIN,QUIT,B,H,FX1,FX2,FY
      CHARACTER(LEN=*),INTENT(INOUT) ::  QTXT                                          
      INTEGER,INTENT(IN) :: IPOS,ITOP
      INTEGER :: IPLOT
      
      IF(ITOP.EQ.1) THEN
         IF(IPOS.EQ.2) THEN
            IF(QIN.NE.0) CALL ARROWTEXT(FX1*B,(FY+0.1)*H,FX1*B,FY*H,&
                                        FX1*B,(FY-0.01)*H,&
                                        IPOS,90.0,WRGB(0,0,255),QIN,QTXT(1:LEN_TRIM(QTXT)))
            IF(QUIT.NE.0) CALL ARROWTEXT(FX2*B,FY*H,FX2*B,(FY+0.1)*H,&
                                         FX2*B,(FY-0.01)*H,&
                                         IPOS,90.0,WRGB(255,0,0),ABS(QUIT),QTXT(1:LEN_TRIM(QTXT)))
         ELSE IF(IPOS.EQ.0) THEN
            IF(QIN.NE.0) CALL ARROWTEXT(FX1*B,(FY+0.1)*H,FX1*B,FY*H,&
                                        FX1*B,(FY+0.11)*H,&
                                        IPOS,90.0,WRGB(0,0,255),QIN,QTXT(1:LEN_TRIM(QTXT)))
            IF(QUIT.NE.0) CALL ARROWTEXT(FX2*B,FY*H,FX2*B,(FY+0.1)*H,&
                                         FX2*B,(FY+0.11)*H,&
                                        IPOS,90.0,WRGB(255,0,0),ABS(QUIT),QTXT(1:LEN_TRIM(QTXT)))
         ENDIF
      ELSE
         IF(IPOS.EQ.2) THEN
            IF(QIN.NE.0) CALL ARROWTEXT(FX1*B,FY*H,FX1*B,(FY+0.1)*H,&
                                        FX1*B,(FY-0.01)*H,&
                                        IPOS,90.0,WRGB(0,0,255),QIN,QTXT(1:LEN_TRIM(QTXT)))
            IF(QUIT.NE.0) CALL ARROWTEXT(FX2*B,(FY+0.1)*H,FX2*B,FY*H,&
                                         FX2*B,(FY-0.01)*H,&
                                         IPOS,90.0,WRGB(255,0,0),ABS(QUIT),QTXT(1:LEN_TRIM(QTXT)))
         ELSE IF(IPOS.EQ.0) THEN
            IF(QIN.NE.0) CALL ARROWTEXT(FX1*B,FY*H,FX1*B,(FY+0.1)*H,&
                                        FX1*B,(FY+0.09)*H,&
                                        IPOS,90.0,WRGB(0,0,255),QIN,QTXT(1:LEN_TRIM(QTXT)))
            IF(QUIT.NE.0) CALL ARROWTEXT(FX2*B,(FY+0.1)*H,FX2*B,FY*H,&
                                         FX2*B,(FY+0.09)*H,&
                                        IPOS,90.0,WRGB(255,0,0),ABS(QUIT),QTXT(1:LEN_TRIM(QTXT)))
         ELSE IF(IPOS.EQ.3) THEN
            IF(QIN.NE.0) CALL ARROWTEXT(FX1*B,FY*H,FX1*B,(FY+0.1)*H,&
                                        (FX1-0.022)*B,(FY+0.12)*H,&
                                        IPOS,90.0,WRGB(0,0,255),QIN,QTXT(1:LEN_TRIM(QTXT)))
            IF(QUIT.NE.0) CALL ARROWTEXT(FX2*B,(FY+0.1)*H,FX2*B,FY*H,&
                                        (FX2+0.022)*B,(FY+0.12)*H,&
                                        IPOS,90.0,WRGB(255,0,0),ABS(QUIT),QTXT(1:LEN_TRIM(QTXT)))

         ENDIF
      ENDIF
      
      END SUBROUTINE VTERM2

!###====================================================================
      SUBROUTINE OUTVALUE(X,Y,VALUE,ITEM)
!###====================================================================
      IMPLICIT NONE
      REAL,INTENT(IN) :: X,Y,VALUE
      CHARACTER(LEN=*),INTENT(IN) :: ITEM
      INTEGER :: ILAST
      CHARACTER(LEN=8) :: FMT
      CHARACTER(LEN=10) :: STR

      FMT=MAKEFMT(VALUE)
      CALL IGRCOLOURN(WRGB(0,0,0))
      IF(INDEX(FMT,'I10').GT.0) THEN
         WRITE(STR,FMT) INT(VALUE)
         CALL LASTCHAR(STR,' ',ILAST); ILAST=MAX0(ILAST,1)
         CALL WGRTEXTSTRING(X,Y,ITEM//'='//STR(ILAST:10) )
      ELSE
         WRITE(STR,FMT) VALUE
         CALL LASTCHAR(STR,' ',ILAST); ILAST=MAX0(ILAST,1)
         CALL WGRTEXTSTRING(X,Y,ITEM//'='//STR(ILAST:10))
      ENDIF

      END SUBROUTINE OUTVALUE

!###====================================================================
      SUBROUTINE CSIZE(CS,CW,CH,IPLOT)
!###====================================================================
      IMPLICIT NONE
      REAL,INTENT(INOUT) :: CW,CH
      INTEGER,INTENT(IN) :: IPLOT
      REAL,INTENT(IN) :: CS
      REAL :: DX,DY

!     CALCULATE PART OF GRAPHICS AREA IN XAND Y IRECTION
      DX=INFOGRAPHICS(9)-INFOGRAPHICS(7)
      DY=INFOGRAPHICS(10)-INFOGRAPHICS(8)
      CH=2.5*CS*REAL(WINFOBITMAP(IPLOT,1))/(DY*REAL(WINFOBITMAP(IPLOT,2)))
      CW=CS/DX

      END SUBROUTINE CSIZE

!###====================================================================
      SUBROUTINE PLOTTREE(X,Y,H,F)
!###====================================================================
      IMPLICIT NONE
      REAL,INTENT(IN) :: F,H,Y,X
      REAL :: R,DX,DY,R0,X0,Y0    
      INTEGER :: I,IGRE,IGRE0,K

      IGRE0=50+(IRANDOMNUMBER(1))*100
      R0=H/3
      X0=X
      Y0=(Y+H-R0)
      CALL IGRCOLOURMODEL(24)
      CALL IGRFILLPATTERN(4,4,4)
      CALL IGRCOLOURN(WRGB(100,0,0))
      CALL IGRRECTANGLE(X0-R0/10,Y,X0+R0/10,Y0)
      CALL IGRFILLPATTERN(0,0,0)
      DO I=1,1000
         R=(IRANDOMNUMBER(1)-0.5)*2*R0/2
         IGRE=10+(IRANDOMNUMBER(1))*190
         CALL IGRCOLOURN(WRGB(0,IGRE,0))
         DX=(IRANDOMNUMBER(1)-0.5)*2*R0
         DY=(IRANDOMNUMBER(1)-0.5)*2*R0
         IF(DX**2+DY**2.LT.R0**2) THEN
             CALL IGRELLIPSE(X0-DX,Y0-DY,ABS(R),F)
             CALL IGRCIRCLE(X0-DX,Y0-DY,ABS(R))
         ENDIF
      ENDDO

!     APPLES IN THE TREE
      CALL IGRFILLPATTERN(4,4,4)
      DO I=1,20
         DX=(IRANDOMNUMBER(1)-0.5)*2*R0
         DY=(IRANDOMNUMBER(1)-0.5)*2*R0
         IGRE=125+(IRANDOMNUMBER(1))*125
         CALL IGRCOLOURN(WRGB(255,IGRE,0))
         CALL IGRCIRCLE(X0-DX,Y0-DY,H/40)
      ENDDO
     
      CALL IGRCOLOURN(WRGB(0,0,0))
      DO K=1,20
         DX=(IRANDOMNUMBER(1)-0.5)*2*R0
         CALL IGRJOIN(X,Y,X-DX,Y-IRANDOMNUMBER(1)*R0)
      ENDDO
      DO K=1,20
         DX=(IRANDOMNUMBER(1)-0.5)*2*R0*0.5
         CALL IGRJOIN(X,Y,X-DX,Y-IRANDOMNUMBER(1)*R0)
      ENDDO

      END SUBROUTINE PLOTTREE

!###====================================================================
      SUBROUTINE LASTCHAR(STR,CH,ILAST)
!###====================================================================
      IMPLICIT NONE
      INTEGER :: IL,I
      INTEGER,INTENT(INOUT) :: ILAST                 
      CHARACTER(LEN=*) :: STR
      CHARACTER(LEN=1),INTENT(IN) :: CH                   

      IL=LEN_TRIM(STR)                   
      ILAST=0                          
      DO I=IL,1,-1                     
         IF(STR(I:I).EQ.CH) THEN       
            ILAST=I      
            RETURN              
         ENDIF                         
      ENDDO                            

      END SUBROUTINE LASTCHAR

!###====================================================================
      SUBROUTINE PLOTSUBREGIO(II,JJ,QIN,QUIT,REGNAME,IPOL,JPOL,NPOL,ICOLPLG,IPLG,DH,B,H,ifit)
!###====================================================================
      IMPLICIT NONE
      INTEGER,INTENT(IN),DIMENSION(NPOL) :: ICOLPLG,IPLG
      INTEGER,INTENT(IN) :: JPOL,IPOL,NPOL
      REAL,INTENT(IN) :: QIN,QUIT,B,H,DH
      REAL :: DX,DY
      INTEGER :: I,J,II,JJ,IPOS,ILEN,IFIT
      CHARACTER(LEN=256) STR
      CHARACTER(LEN=10) STRDUM
      CHARACTER(LEN=8) :: FMT
      CHARACTER(LEN=256) :: REGNAME

      STR=' '; STRDUM=' '; IPOS=1; WRITE(STRDUM(1:7),'(I7)') IPLG(JPOL)
      DO J=1,7
         IF(STRDUM(J:J).EQ.' ') IPOS=J+1
      ENDDO

      ILEN=MIN0(1+(7-IPOS+1)+5+LEN_TRIM(REGNAME)+1,256)
      STR(1:ILEN)='('//STRDUM(IPOS:7)//') - ('//REGNAME(1:LEN_TRIM(REGNAME))//')'

      IF(QIN.GT.0) THEN
         II=II+1
         CALL ARROWTEXT(0.15*B,0.65*H-II*DH,0.2*B,0.65*H-II*DH,&
                       0.15*B,0.65*H+DH/2-II*DH,&
                       0,0.0,WRGB(0,0,255),QIN,STR(1:ILEN))
         if(ifit.eq.0) then
            CALL PASTEL(ICOLPLG(JPOL),0.6); CALL IGRCIRCLE(0.14*B,0.65*H-II*DH,0.006*B)
         else
            CALL igrcolourn(wrgb(255,125,0)); CALL IGRCIRCLE(0.14*B,0.65*H-II*DH,0.006*B)
         endif
         CALL IGRCOLOURN(WRGB(255,125,0)); CALL IGRCIRCLE(0.21*B,0.65*H-II*DH,0.006*B)

         CALL IGRFILLPATTERN(0,0,0)
         CALL IGRLINETYPE(0)
         CALL IGRCOLOURN(WRGB(0,0,0))
         CALL IGRCIRCLE(0.14*B,0.65*H-II*DH,0.006*B)
         CALL IGRCIRCLE(0.21*B,0.65*H-II*DH,0.006*B)
      ENDIF

      STR=' '; STRDUM=' '; IPOS=1; WRITE(STRDUM(1:7),'(I7)') IPLG(JPOL)
      DO J=1,7
         IF(STRDUM(J:J).EQ.' ') IPOS=J+1
      ENDDO

      ILEN=MIN0(1+LEN_TRIM(REGNAME)+5+(7-IPOS+1)+1,256)
      STR(1:ILEN)='('//REGNAME(1:LEN_TRIM(REGNAME))//') - ('//STRDUM(IPOS:7)//')'
      
      IF(QUIT.LT.0) THEN
         JJ=JJ+1
         CALL ARROWTEXT(0.78*B,0.65*H-JJ*DH,0.83*B,0.65*H-JJ*DH,&
                        0.78*B,0.65*H+DH/2-JJ*DH,&
                        0,0.0,WRGB(255,0,0),ABS(QUIT),STR(1:ILEN))

         CALL IGRCOLOURN(WRGB(255,125,0)); CALL IGRCIRCLE(0.77*B,0.65*H-JJ*DH,0.006*B)
         if(ifit.eq.0) then
            CALL PASTEL(ICOLPLG(JPOL),0.6); CALL IGRCIRCLE(0.84*B,0.65*H-JJ*DH,0.006*B)
         else
            CALL igrcolourn(wrgb(255,125,0)); CALL IGRCIRCLE(0.84*B,0.65*H-JJ*DH,0.006*B)
         endif

         CALL IGRFILLPATTERN(0,0,0)
         CALL IGRCOLOURN(WRGB(0,0,0))
         CALL IGRLINETYPE(0)
         CALL IGRCIRCLE(0.84*B,0.65*H-JJ*DH,0.006*B)
         CALL IGRCIRCLE(0.77*B,0.65*H-JJ*DH,0.006*B)
      ENDIF
      
      END SUBROUTINE PLOTSUBREGIO

!###====================================================================
      SUBROUTINE PLOTIDF(IIPOL,NPOL,IPLG,ICOLPLG,IPLOT,LOCAL,AREA,IDF,LSUM,SUMNR)
!###====================================================================
      IMPLICIT NONE
      INTEGER,INTENT(IN),DIMENSION(:) :: IIPOL
      REAL,INTENT(OUT) :: AREA
      INTEGER,DIMENSION(NPOL),INTENT(IN) :: IPLG,ICOLPLG
      INTEGER,INTENT(IN) :: IPLOT,SUMNR
      TYPE (IDFOBJ),INTENT(IN) :: IDF
      INTEGER :: IREGULAR,IREC,I,J,IPOL,KPOL,NCOL,NROW,NPOL,K,II,ISEL
      INTEGER :: IR,IG,IB
      REAL :: X1PLOT,X0PLOT,Y1PLOT,Y0PLOT,X,Y,VALUE,VALUEBUUR,X0,X1,Y0,Y1
      REAL :: FAREAX1,FAREAX2,FAREAY1,FAREAY2,DX,DY,DXWIN,DYWIN
      LOGICAL :: LOCAL,LSUM

!     PLOT REGIONS
      FAREAX1=0.68
      FAREAX2=0.85
      AREA=0.0

!     LONELY CELLS?
!     dit zijn cellen die ingelosten zitten tussen regio's 
!     cel(i-1,j),cel(i,j),cel(i+1,j)
!     cel(i,j-1),cel(i,j),cel(i,j+1)   
      !DO J=1,IDF%NROW
      !   DO I=1,IDF%NCOL
      !   IF(I.GT.1.AND.I.LT.IDF%NCOL.AND.J.GT.1.AND.J.LT.IDF%NROW) THEN
      !      IF(ABS(NINT(IDF%X(I-1,J))).NE.ABS(NINT(IDF%X(I,J))).AND.&
      !         ABS(NINT(IDF%X(I+1,J))).NE.ABS(NINT(IDF%X(I,J)))) THEN
      !         IDF%X(I,J)=IDF%X(I+1,J)
      !      ENDIF
      !      IF(ABS(NINT(IDF%X(I,J-1))).NE.ABS(NINT(IDF%X(I,J))).AND.&
      !         ABS(NINT(IDF%X(I,J+1))).NE.ABS(NINT(IDF%X(I,J)))) THEN
      !         IDF%X(I,J)=IDF%X(I,J+1)
      !      ENDIF
      !   ENDIF
      !ENDDO
      !ENDDO

!     CENTRE AREA OF INTEREST
      IF(LOCAL) THEN
!        ONE CAN CHOOSE TO PLOT THE REGIONS FOR JUST THE ENVELOPING RECTANGLE
!        THEREFORE THE SIZE OF THIS AREA SHOULD BE CALCULATED
         X0PLOT=IDF%XMAX; X1PLOT=IDF%XMIN 
         Y0PLOT=IDF%YMAX; Y1PLOT=IDF%YMIN 
         DO J=1,IDF%NROW
          DO I=1,IDF%NCOL
           IF(LSUM) THEN
              DO II=1,SIZE(IIPOL)
               IPOL=IIPOL(II)
               IF(ABS(NINT(IDF%X(I,J))).EQ.IPLG(IPOL)) THEN
                  X0PLOT=AMIN1(X0PLOT,IDF%SX(I-1)); X1PLOT=AMAX1(X1PLOT,IDF%SX(I)) 
                  Y0PLOT=AMIN1(Y0PLOT,IDF%SY(J-1)); Y1PLOT=AMAX1(Y1PLOT,IDF%SY(J)) 
               ENDIF
              ENDDO
           ELSE
               IF(ABS(NINT(IDF%X(I,J))).EQ.SUMNR) THEN
                 X0PLOT=AMIN1(X0PLOT,IDF%SX(I-1)); X1PLOT=AMAX1(X1PLOT,IDF%SX(I)) 
                 Y0PLOT=AMIN1(Y0PLOT,IDF%SY(J-1)); Y1PLOT=AMAX1(Y1PLOT,IDF%SY(J)) 
               ENDIF
           ENDIF
          ENDDO
         ENDDO
         CALL MAKESQUAREWINDOW(X0PLOT,Y0PLOT,X1PLOT,Y1PLOT)
      ELSE
         X0PLOT=IDF%XMIN; X1PLOT=IDF%XMAX 
         Y0PLOT=IDF%YMIN; Y1PLOT=IDF%YMAX 
         CALL MAKESQUAREWINDOW(X0PLOT,Y0PLOT,X1PLOT,Y1PLOT)
      ENDIF

!     NUMBER OF PIXELS X-DIRECTION= (FX2-FX1)*NXPIXEL
!     CALCULATE AREASIZE IN Y-DIRECTION (FX2-FX1)*NXPIXEL/NYPIXEL
      FAREAY2=FAREAX2
      FAREAY1=FAREAY2-((FAREAX2-FAREAX1)*REAL(WINFOBITMAP(IPLOT,1))/REAL(WINFOBITMAP(IPLOT,2)))*(Y1PLOT-Y0PLOT)/(X1PLOT-X0PLOT)

      FAREAY1=0.01
      FAREAY2=FAREAY1+((FAREAX2-FAREAX1)*REAL(WINFOBITMAP(IPLOT,1))/REAL(WINFOBITMAP(IPLOT,2)))*(Y1PLOT-Y0PLOT)/(X1PLOT-X0PLOT)

      CALL IGRAREA(FAREAX1,FAREAY1,FAREAX2,FAREAY2)
      CALL IGRUNITS(X0PLOT,Y0PLOT,X1PLOT,Y1PLOT)
      CALL IGRFILLPATTERN(4,4,4)
!     PLOT THE DIFFERENT REGIONS IN DIFFERENT COLORS
      DO J=1,IDF%NROW
       DO I=1,IDF%NCOL
          IF(LSUM) THEN
            DO II=1,SIZE(IIPOL)
               IPOL=IIPOL(II)
               IF(ABS(NINT(IDF%X(I,J))).EQ.IPLG(IPOL)) THEN
                  AREA=AREA+(IDF%SX(I)-IDF%SX(I-1))*(IDF%SY(J-1)-IDF%SY(J))
               ENDIF
            ENDDO
          ELSE
             IF(ABS(NINT(IDF%X(I,J))).EQ.SUMNR) THEN
                AREA=AREA+(IDF%SX(I)-IDF%SX(I-1))*(IDF%SY(J-1)-IDF%SY(J))
             ENDIF
          ENDIF
          ISEL=0
          IF(LSUM) THEN
             DO K=1,SIZE(IIPOL)
                 KPOL=IIPOL(K)
                 IF(ABS(NINT(IDF%X(I,J))).EQ.IPLG(KPOL)) THEN
                    ISEL=1
                 ENDIF
             ENDDO
          ELSE
             IF(ABS(NINT(IDF%X(I,J))).EQ.SUMNR) THEN
             	ISEL=1
             ENDIF
          ENDIF

          IF(ISEL.EQ.1) THEN
             CALL IGRCOLOURN(WRGB(255,125,0))
          ELSE
!            MAKE COLORS "NEIGHBOR-REGIONS" LESS INTENSE
             DO K=1,NPOL
                IF(ABS(NINT(IDF%X(I,J))).EQ.IPLG(K)) CALL PASTEL(ICOLPLG(K),0.6)
             ENDDO
          ENDIF
          if(IDF%X(I,J).ne.idf%nodata) CALL IGRRECTANGLE(IDF%SX(I-1),IDF%SY(J-1),IDF%SX(I),IDF%SY(J))
       ENDDO
      ENDDO

      CALL IGRCOLOURN(WRGB(0,0,0))
!     plot a black border around the selected region, summed (lsum=.true. or unique lsum=.false. and sumnr is region
      DO J=1,IDF%NROW
         DO I=1,IDF%NCOL
            IF(LSUM) THEN
                DO II=1,SIZE(IIPOL)
                   IPOL=IIPOL(II)
                   IF(I.GT.1.AND.I.LT.IDF%NCOL.AND.ABS(NINT(IDF%X(I,J))).EQ.IPLG(IPOL)) THEN
                       IF(ABS(NINT(IDF%X(I-1,J))).NE.ABS(NINT(IDF%X(I,J)))) CALL IGRJOIN(IDF%SX(I-1),IDF%SY(J-1),IDF%SX(I-1),IDF%SY(J))
                       IF(ABS(NINT(IDF%X(I+1,J))).NE.ABS(NINT(IDF%X(I,J)))) CALL IGRJOIN(IDF%SX(I),  IDF%SY(J-1),IDF%SX(I),  IDF%SY(J))
                   ENDIF                  
                   IF(J.GT.1.AND.J.LT.IDF%NROW.AND.ABS(NINT(IDF%X(I,J))).EQ.IPLG(IPOL)) THEN
                       IF(ABS(NINT(IDF%X(I,J-1))).NE.ABS(NINT(IDF%X(I,J)))) CALL IGRJOIN(IDF%SX(I-1),IDF%SY(J-1),IDF%SX(I),IDF%SY(J-1))
                       IF(ABS(NINT(IDF%X(I,J+1))).NE.ABS(NINT(IDF%X(I,J)))) CALL IGRJOIN(IDF%SX(I-1),IDF%SY(J),  IDF%SX(I),IDF%SY(J))
                   ENDIF
                   IF(I.EQ.1.AND.ABS(NINT(IDF%X(I,J))).EQ.IPLG(IPOL)) CALL IGRJOIN(IDF%SX(I-1),IDF%SY(J-1),IDF%SX(I-1),IDF%SY(J))
                   IF(I.EQ.IDF%NCOL.AND.ABS(NINT(IDF%X(I,J))).EQ.IPLG(IPOL)) CALL IGRJOIN(IDF%SX(I),  IDF%SY(J-1),IDF%SX(I),  IDF%SY(J))
                   IF(J.EQ.1.AND.ABS(NINT(IDF%X(I,J))).EQ.IPLG(IPOL)) CALL IGRJOIN(IDF%SX(I-1),IDF%SY(J-1),IDF%SX(I),  IDF%SY(J-1))
                   IF(J.EQ.IDF%NROW.AND.ABS(NINT(IDF%X(I,J))).EQ.IPLG(IPOL)) CALL IGRJOIN(IDF%SX(I-1),IDF%SY(J),  IDF%SX(I),  IDF%SY(J))
                ENDDO
            ELSE
                IF(I.GT.1.AND.I.LT.IDF%NCOL.AND.ABS(NINT(IDF%X(I,J))).EQ.SUMNR) THEN
                    IF(ABS(NINT(IDF%X(I-1,J))).NE.ABS(NINT(IDF%X(I,J)))) CALL IGRJOIN(IDF%SX(I-1),IDF%SY(J-1),IDF%SX(I-1),IDF%SY(J))
                    IF(ABS(NINT(IDF%X(I+1,J))).NE.ABS(NINT(IDF%X(I,J)))) CALL IGRJOIN(IDF%SX(I),  IDF%SY(J-1),IDF%SX(I),  IDF%SY(J))
                ENDIF                  
                IF(J.GT.1.AND.J.LT.IDF%NROW.AND.ABS(NINT(IDF%X(I,J))).EQ.SUMNR) THEN
                    IF(ABS(NINT(IDF%X(I,J-1))).NE.ABS(NINT(IDF%X(I,J)))) CALL IGRJOIN(IDF%SX(I-1),IDF%SY(J-1),IDF%SX(I),IDF%SY(J-1))
                    IF(ABS(NINT(IDF%X(I,J+1))).NE.ABS(NINT(IDF%X(I,J)))) CALL IGRJOIN(IDF%SX(I-1),IDF%SY(J),  IDF%SX(I),IDF%SY(J))
                ENDIF
                IF(I.EQ.1.AND.ABS(NINT(IDF%X(I,J))).EQ.SUMNR) CALL IGRJOIN(IDF%SX(I-1),IDF%SY(J-1),IDF%SX(I-1),IDF%SY(J))
                IF(I.EQ.IDF%NCOL.AND.ABS(NINT(IDF%X(I,J))).EQ.SUMNR) CALL IGRJOIN(IDF%SX(I),  IDF%SY(J-1),IDF%SX(I),  IDF%SY(J))
                IF(J.EQ.1.AND.ABS(NINT(IDF%X(I,J))).EQ.SUMNR) CALL IGRJOIN(IDF%SX(I-1),IDF%SY(J-1),IDF%SX(I),  IDF%SY(J-1))
                IF(J.EQ.IDF%NROW.AND.ABS(NINT(IDF%X(I,J))).EQ.SUMNR) CALL IGRJOIN(IDF%SX(I-1),IDF%SY(J),  IDF%SX(I),  IDF%SY(J))
            ENDIF
         ENDDO
      ENDDO

      CALL IGRCOLOURN(WRGB(0,0,0))
      CALL IGRFILLPATTERN(0,0,0)
      CALL IGRRECTANGLE(X0PLOT,Y0PLOT,X1PLOT,Y1PLOT)
      
      !check..... write idf als code.asc
      !OPEN(UNIT=3000,FILE='CODE.ASC')
      !WRITE(3000,*) 'NCOL',IDF%NCOL
      !WRITE(3000,*) 'NROW',IDF%NROW
      !WRITE(3000,*) 'XLL',IDF%YMAX
      !WRITE(3000,*) 'YLL',IDF%YMIN
      !WRITE(3000,*) 'DXY',IDF%SX(2)-IDF%SX(1)
      !WRITE(3000,*) 'XNODATA',9999.0
      !DO J=1,IDF%NROW
      !   WRITE(3000,'(10F10.2)') (IDF%X(I,J),I=1,IDF%NCOL)
      !ENDDO
      !CLOSE(3000)
      
      END SUBROUTINE PLOTIDF


!###====================================================================
      SUBROUTINE PASTEL(ICOL,FADE)
!###====================================================================
      USE WINTERACTER
      INTEGER :: ICOL,IR,IG,IB
      REAL :: FADE
      CALL WRGBSPLIT(ICOL,IR,IG,IB)
      IR=IR+(255-IR)*FADE
      IG=IG+(255-IG)*FADE
      IB=IB+(255-IB)*FADE
      CALL IGRCOLOURN(WRGB(IR,IG,IB))
      END SUBROUTINE PASTEL



!###====================================================================
      FUNCTION MAKEFMT(VALUE)
!###====================================================================
      IMPLICIT NONE
      REAL,INTENT(IN) :: VALUE
      CHARACTER(LEN=8) :: MAKEFMT

      MAKEFMT='(F10.1) '
      IF(ABS(VALUE).GT.100000)                        MAKEFMT='(G10.4) '
      IF(ABS(VALUE).GT.10  .AND.ABS(VALUE).LE.100000) MAKEFMT='(F10.0) '
      IF(ABS(VALUE).GT.1   .AND.ABS(VALUE).LE.10   )  MAKEFMT='(F10.1) '
      IF(ABS(VALUE).GT.0.01.AND.ABS(VALUE).LE.1)      MAKEFMT='(F10.2) '
      IF(ABS(VALUE).GT.0   .AND.ABS(VALUE).LE.0.01)   MAKEFMT='(G10.4) '

      END FUNCTION  MAKEFMT

!###====================================================================
      INTEGER FUNCTION NNOSPACE(STR)
!###====================================================================
      IMPLICIT NONE
      CHARACTER(LEN=*),INTENT(IN) :: STR
      INTEGER :: I
      NNOSPACE=0
      DO I=1,LEN_TRIM(STR)
         IF(STR(I:I).NE.' ') NNOSPACE=NNOSPACE+1
      ENDDO
      END FUNCTION NNOSPACE

!###====================================================================
      SUBROUTINE MAKESQUAREWINDOW(X0PLOT,Y0PLOT,X1PLOT,Y1PLOT)
!###====================================================================
      IMPLICIT NONE
      REAL, INTENT(INOUT) :: X0PLOT,Y0PLOT,X1PLOT,Y1PLOT
      REAL :: DXWIN,DYWIN

      DXWIN=X1PLOT-X0PLOT
      DYWIN=Y1PLOT-Y0PLOT
      IF(DXWIN/DYWIN.LT.1) THEN
         X0PLOT=X0PLOT-(DYWIN-DXWIN)/2; X1PLOT=X1PLOT+(DYWIN-DXWIN)/2
      ELSE
         Y0PLOT=Y0PLOT-(DXWIN-DYWIN)/2; Y1PLOT=Y1PLOT+(DXWIN-DYWIN)/2
      ENDIF
!     MAKE WINDOW 5% BIGGER
      DXWIN=X1PLOT-X0PLOT
      DYWIN=Y1PLOT-Y0PLOT
      X0PLOT=X0PLOT-0.05*DXWIN; X1PLOT=X1PLOT+0.05*DXWIN;      
      Y0PLOT=Y0PLOT-0.05*DYWIN; Y1PLOT=Y1PLOT+0.05*DYWIN;      

      END SUBROUTINE MAKESQUAREWINDOW

     
END MODULE MOD_WBAL_GRAPHICS


