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
MODULE MOD_WBAL_GRAPHICS

USE WINTERACTER
USE MOD_DBL
USE MOD_UTL, ONLY : UTL_GETFORMAT
USE MOD_IDF_PAR, ONLY : IDFOBJ

CONTAINS
!###====================================================================
      LOGICAL FUNCTION DRAWBAL(Q,QTXT,NXPIX,NYPIX,CS,IIPOL,NPOL,QSUBREGIO,ICOLPLG,IPLG,IDF,LOCAL,GRAPHTITLE,IPLOT,LSUM,STRUNIT,SUMNR)    
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
      REAL(KIND=DP_KIND),DIMENSION(NPOL,2),INTENT(INOUT) :: QSUBREGIO
      REAL(KIND=DP_KIND),DIMENSION(24,2),INTENT(INOUT) :: Q
      CHARACTER(LEN=*),DIMENSION(24),INTENT(INOUT) :: QTXT
      CHARACTER(LEN=*),INTENT(IN) :: GRAPHTITLE
      INTEGER, INTENT(IN) :: NPOL,SUMNR
      INTEGER, INTENT(IN),DIMENSION(:) :: IIPOL
      REAL(KIND=DP_KIND),DIMENSION(4) :: XR,YR
      CHARACTER(LEN=256) :: REGNAME
      CHARACTER(LEN=10) :: STRDUM
      INTEGER :: NXPIX,NYPIX,IPLOT,IUNSATURATED,ISATURATED,IBL1,IGR1,IRD1,IBL2,IGR2,IRD2,&
                 II,JJ,JPOL,I,J,IPOL,JDUM,IPOS,IFIT,JFIT,IXPIX,IYPIX
      REAL(KIND=DP_KIND) :: XPOS1,XPOS2,YPOS,XPIX,YPIX
      REAL(KIND=DP_KIND) :: SUMIN,SUMOUT,AREA,CS,B,H
      REAL(KIND=DP_KIND) :: DW,DH,DY,DX
      LOGICAL :: LOCAL,LSUM
      CHARACTER(LEN=*),INTENT(IN) :: STRUNIT
       
      TYPE (IDFOBJ),INTENT(IN) :: IDF


      DRAWBAL=.FALSE.
      B=10000
      H=10000
!     VERTICAL SHIFT GRAPH
      DX=0.05D0
      DY=0.05D0
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
      CALL DBL_IGRAREA(0.0D0,0.5D0+DY,1.0D0,0.95D0+DY)
      CALL DBL_IGRUNITS(0.0D0,0.0D0,B,1.1D0*H)
      CALL IGRFILLPATTERN(4,4,4)
      CALL IGRCOLOURN(WRGB(255,255,100))

!     check saturated unsaturated zone or both
      IUNSATURATED=0; IF(SUM(Q(14:24,1)).NE.0.OR.SUM(Q(14:24,2)).NE.0) IUNSATURATED=1
      ISATURATED  =0; IF(SUM(Q(1 :13,1)).NE.0.OR.SUM(Q(1 :13,2)).NE.0) ISATURATED=1
!     plotting of unsaturated/satuarted zone is always
      IUNSATURATED=1
      ISATURATED  =1

      IF(IUNSATURATED.NE.0) THEN
         CALL DBL_IGRRECTANGLE((0.2D0-DX)*B,0.2D0*H,(0.8D0+DX)*B,0.8D0*H)
         CALL IGRFILLPATTERN(0,0,0)
         CALL PLOTTREE(0.34D0*B,0.79D0*H,0.15*H,1.1D0)
         CALL PLOTTREE(0.42D0*B,0.79D0*H,0.13D0*H,1.1D0)
      ENDIF
      CALL DBL_IGRAREA(0.2D0-DX,0.48D0+DY ,0.8D0+DX,0.7D0+DY)
      CALL DBL_IGRUNITS(0.0D0,0.0D0,B,H)
!     Unsaturated zone with gradient fill
      IF(IUNSATURATED.EQ.1) THEN
         IRD1=190; IGR1=215;  IBL1=240
         IRD2=255; IGR2=255;  IBL2=100
!        unsaturated zone from yellow to blue.....
         CALL IGRCOLOURN(WRGB(IRD1,IGR1,IBL1))
         CALL IGRCOLOURN(WRGB(IRD2,IGR2,IBL2))
         XR(1)=0.0D0; YR(1)=0.0D0
         XR(2)=B; YR(2)=YR(1)
         XR(3)=B; YR(3)=H
         XR(4)=0; YR(4)=YR(3)
         CALL IGRPOLYGONGRAD(REAL(XR,4),REAL(YR,4),4,1)
      ENDIF

      CALL DBL_IGRAREA(0.0D0,0.0D0+DY,1.0D0,0.7D0+DY)
      CALL DBL_IGRUNITS(0.0D0,0.0D0,B,1.1D0*H)

!     PLOT separating layers
      IF(ISATURATED.EQ.1) THEN

         CALL IGRFILLPATTERN(4,4,4)
         CALL IGRCOLOURN(WRGB(190,215,240))
         CALL DBL_IGRRECTANGLE((0.2D0-DX)*B,0.18D0*H,(0.8D0+DX)*B,0.82D0*H)
         CALL IGRFILLPATTERN(0,0,0)
         CALL IGRLINEWIDTH(3)
         IF(Q(9,1).NE.0.0D0.OR.Q(9,2).NE.0.0D0) THEN
            CALL IGRFILLPATTERN(4,4,4)
            CALL IGRCOLOURN(WRGB(195,225,180))
            CALL DBL_IGRRECTANGLE((0.2D0-DX)*B,0.18D0*H,(0.8D0+DX)*B,0.22D0*H)
            CALL IGRFILLPATTERN(0,0,0)
            CALL IGRCOLOURN(WRGB(  0,155,  0))
            CALL DBL_IGRRECTANGLE((0.2D0-DX)*B,0.18D0*H,(0.8D0+DX)*B,0.22D0*H)
         ENDIF
         IF(Q(10,1).NE.0.0D0.OR.Q(10,2).NE.0.0D0) THEN
            CALL IGRFILLPATTERN(4,4,4)
            CALL IGRCOLOURN(WRGB(195,225,180))
            CALL DBL_IGRRECTANGLE((0.2D0-DX)*B,0.78D0*H,(0.8D0+DX)*B,0.82D0*H)
            CALL IGRFILLPATTERN(0,0,0)
            CALL IGRCOLOURN(WRGB(  0,155,  0))
            CALL DBL_IGRRECTANGLE((0.2D0-DX)*B,0.78D0*H,(0.8D0+DX)*B,0.82D0*H)
         ENDIF
      ENDIF
      CALL CSIZE(CS,DW,DH,IPLOT)
      CALL DBL_WGRTEXTFONT(IFAMILY=102,ISTYLE=1,TWIDTH=DW,THEIGHT=DH)
      CALL PLOTDRAIN(Q(1,1),Q(1,2),0.22D0,0.75D0,0.015D0,QTXT(1),B,H)                                ! PLOT DRN+OLF FLOW
      CALL PLOTDRAIN(Q(2,1),Q(2,2),0.19D0,0.75D0,0.015D0,QTXT(2),B,H)                                ! PLOT DRN+OLF FLOW

      CALL PLOTRIV(Q(3,1),Q(3,2),0.27D0,WRGB(100,100,255),QTXT(3),B,H)                           ! PLOT RIV FLOW
      CALL PLOTRIV(Q(5,1),Q(5,2),0.34D0,WRGB(150,150,255),QTXT(5),B,H)                           ! PLOT ISG FLOW
      CALL PLOTRIV(Q(4,1),Q(4,2),0.41D0,WRGB(100,255,100),QTXT(4),B,H)                           ! PLOT GHB FLOW

      CALL PLOTWEL(Q(6,1),Q(6,2),0.62D0,QTXT(6),B,H)                                             ! PLOT WELL FLOW
      CALL PLOTREG(Q(7,1),Q(7,2),0.66D0,QTXT(7),B,H)                                             ! PLOT REGIO FLOW
      CALL PLOTREG(Q(8,1),Q(8,2),0.25D0,QTXT(8),B,H)                                             ! PLOT CONSTANT HEAD FLOW

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
            WRITE(REGNAME(II+1:II+7-IPOS+2),'(A,A)') STRDUM(IPOS:7),','
            II=II+7-IPOS+2
         ENDDO
         II=LEN_TRIM(REGNAME); REGNAME(II:II)=' '
      ELSE
         WRITE(STRDUM(1:7),'(I7)') SUMNR
         DO JJ=1,7
            IF(STRDUM(JJ:JJ).EQ.' ') IPOS=JJ+1
         ENDDO
         WRITE(REGNAME(II+1:II+7-IPOS+2),'(A,A)') STRDUM(IPOS:7),','
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
           CALL PLOTSUBREGIO(II,JJ,-QSUBREGIO(JPOL,2),-QSUBREGIO(JPOL,1),REGNAME,JPOL,NPOL,ICOLPLG,IPLG,DH,B,H,IFIT)

!          testing.....calculate width of plotted text starting at position x=0.13*b, y=0.65*h-(ii-0.5)*dh, until the centre....nxpix/2
!          left side of plot, calculating width of box
           CALL DBL_IGRUNITSTOPIXELS(0.13D0*B,0.65D0*H-(II-0.5D0)*DH,IXPIX,IYPIX)
           DO I=IXPIX,NXPIX/2
              CALL DBL_IGRUNITSFROMPIXELS(I,IYPIX,XPIX,YPIX)
              IF(IGRGETPIXEL(REAL(XPIX,4),REAL(YPIX,4)).EQ.WRGB(0,0,0)) THEN
              	 YPOS=YPIX; IF(XPIX.GT.XPOS1) XPOS1=XPIX
              ENDIF
           ENDDO
!          right side of plot, calculating width of box
           CALL DBL_IGRUNITSTOPIXELS(0.76D0*B,0.65D0*H-(II-0.5D0)*DH,IXPIX,IYPIX)
           DO I=IXPIX,NXPIX
              CALL DBL_IGRUNITSFROMPIXELS(I,IYPIX,XPIX,YPIX)
              IF(IGRGETPIXEL(REAL(XPIX,4),REAL(YPIX,4)).EQ.WRGB(0,0,0)) THEN
              	 YPOS=YPIX; IF(XPIX.GT.XPOS2) XPOS2=XPIX
              ENDIF
           ENDDO

           JDUM=JPOL
        ENDIF
        ENDDO
      ENDDO
      CALL DVOLUME(Q(18,1),Q(18,2),0.70D0,QTXT(18),B,H)                        ! PLOT STORAGE FLOW


      CALL IGRLINETYPE(2)
      CALL IGRCOLOURN(WRGB(0,0,0))
      CALL IGRFILLPATTERN(0,0,0)
!     PLOT DASHED RECTANGLE AROUND SUBREGIOFLOW
      IF(II.GT.0) CALL DBL_IGRRECTANGLE(0.13*B,0.65*H,xpos1+0.01D0*B,0.65*H-(II+0.5)*DH)
      IF(JJ.GT.0) CALL DBL_IGRRECTANGLE(0.76*B,0.65*H,xpos2+0.01D0*B,0.65*H-(JJ+0.5)*DH)

      CALL IGRLINETYPE(0)
      CALL VTERM2(Q(11,1),Q(11,2),0.56D0, 0.58D0, 0.8D0,  QTXT(11),B,H,2,1)       ! PLOT RCH FLOW
      CALL VTERM2(Q(12,1),Q(12,2),0.62D0, 0.64D0, 0.8D0,  QTXT(12),B,H,2,1)       ! PLOT EVT FLOW
      CALL VTERM2(Q(13,1),Q(13,2),0.68D0, 0.70D0, 0.8D0,  QTXT(13),B,H,2,1)       ! PLOT CAP FLOW
      CALL VTERM2(Q(10,1),Q(10,2),0.74D0, 0.76D0, 0.8D0,  QTXT(9), B,H,2,1)       ! PLOT FLF FLOW (FLOW TOP FACE)
      CALL VTERM2(Q(9,1) ,Q(9,2) ,0.49D0, 0.51D0, 0.22D0, QTXT(10),B,H,3,0)      ! PLOT FLF FLOW (FLOW LOWER FACE)

!     *********************UNSATURATED*********************
      CALL DBL_IGRAREA(0.0D0,0.50D0+dy,1.0D0,0.95D0+dy)
      CALL DBL_IGRUNITS(0.0D0,0.0D0,B,1.1D0*H)
      CALL IGRFILLPATTERN(0,0,0)
 
      CALL CSIZE(CS,DW,DH,IPLOT)
      CALL DBL_WGRTEXTFONT(IFAMILY=102,ISTYLE=1,TWIDTH=DW,THEIGHT=DH)
      CALL VTERM2(Q(14,1),Q(14,2),0.50D0,0.52D0,0.8D0,QTXT(14),B,H,2,1)            ! PLOT METASWAP ETACT       
      CALL VTERM2(Q(15,1),Q(15,2),0.56D0,0.58D0,0.8D0,QTXT(15),B,H,2,1)            ! PLOT METASWAP PM
      CALL VTERM2(Q(23,1),Q(23,2),0.62D0,0.64D0,0.8D0,QTXT(23),B,H,2,1)            ! PLOT METASWAP QRUN  
      CALL VTERM2(Q(17,1),Q(17,2),0.68D0,0.70D0,0.8D0,QTXT(17),B,H,2,1)            ! PLOT METASWPP PMSW  
      CALL VTERM2(Q(16,1),Q(16,2),0.74D0,0.76D0,0.8D0,QTXT(16),B,H,2,1)            ! PLOT METASWPP PMGW  
                                                                                           
      CALL DVOLUME(Q(19,1)  ,Q(19,2), 0.47D0,               QTXT(19),B,H)           ! PLOT METASWAP STORAGE
      CALL DVOLUME(Q(21,1)  ,Q(21,1), 0.42D0,               QTXT(21),B,H)           ! PLOT METASWAP QCOR
      CALL PLOTDRAIN(Q(22,1),Q(22,2), 0.22D0,0.50D0,0.01D0, QTXT(22),B,H)           ! PLOT METASWAP QDR
      CALL PLOTDRAIN(Q(20,1),Q(20,2), 0.25D0,0.50D0,0.01D0, QTXT(20),B,H)           ! PLOT METASWPA QSPGW
      CALL VTERM2(Q(24,1)   ,Q(24,2), 0.48D0,0.50D0,0.0D0, QTXT(24),B,H,0,0)       ! PLOT METASWAP QMODF

      CALL PLOTIDF(IIPOL,NPOL,IPLG,ICOLPLG,IPLOT,LOCAL,AREA,IDF,LSUM,SUMNR)

      CALL DBL_IGRAREA(0.0D0,0.0D0,1.0D0,1.0D0)
      CALL DBL_IGRUNITS(0.0D0,0.0D0,1.0D0,1.0D0)
      CALL DBL_WGRTEXTORIENTATION(IALIGN=0,ANGLE=0.0D0,IDIR=0)
      
      CALL IGRCOLOURN(WRGB(0,0,0))
      CALL CSIZE(CS,DW,DH,IPLOT)
      CALL DBL_WGRTEXTFONT(IFAMILY=101,ISTYLE=1,TWIDTH=DW,THEIGHT=DH)
     
      CALL DBL_WGRTEXTSTRING(0.15D0,0.03D0+4.5D0*DH,'WATERBALANCE REGION    '//TRIM(REGNAME)) 
      CALL OUTVALUE(0.15D0,0.03D0+3.5D0*DH,AREA/10000.0D0,          'AREA             HA   ')
      CALL OUTVALUE(0.15D0,0.03D0+2.5D0*DH,SUMIN ,        'TOTAL SUM IN     '//STRUNIT)
      CALL OUTVALUE(0.15D0,0.03D0+1.5D0*DH,SUMOUT,        'TOTAL SUM OUT    '//STRUNIT)
      CALL OUTVALUE(0.15D0,0.03D0+0.5D0*DH,SUMOUT+SUMIN,  'TOTAL SUM OUT+IN '//STRUNIT)
      CALL CSIZE(1.5D0*CS,DW,DH,IPLOT)
      CALL DBL_WGRTEXTFONT(IFAMILY=102,ISTYLE=1,TWIDTH=DW,THEIGHT=DH)
      CALL DBL_WGRTEXTORIENTATION(IALIGN=1,ANGLE=0.0D0,IDIR=0)
      CALL DBL_WGRTEXTBLOCK(0.15D0,0.95D0,0.85D0,1.0D0,GRAPHTITLE(1:LEN_TRIM(GRAPHTITLE)),1.0D0,4)
      CALL IGRLINEWIDTH(2)
      CALL DBL_IGRRECTANGLE(0.0D0,0.0D0,1.0D0,1.0D0)
      CALL IGRLINEWIDTH(1)
      DRAWBAL=.TRUE.

      END FUNCTION DRAWBAL

!###====================================================================
      SUBROUTINE ARROWTEXT(X0,Y0,X1,Y1,XTXT,YTXT,IJUSTIFY,ANGLE,ICOLOR,Q,QTXT)
!###====================================================================
      IMPLICIT NONE
      REAL(KIND=DP_KIND),INTENT(IN) :: X0,Y0,X1,Y1,XTXT,YTXT,ANGLE,Q
      INTEGER,INTENT(IN) :: IJUSTIFY,ICOLOR
      CHARACTER(LEN=*),INTENT(INOUT) :: QTXT
      
      CALL IGRFILLPATTERN(4,4,4)
      CALL IGRCOLOURN(ICOLOR)
      CALL DBL_IGRARROWJOIN(X0,Y0,X1,Y1,2)
      CALL DBL_WGRTEXTORIENTATION(IJUSTIFY,ANGLE,0)
      CALL OUTVALUE(XTXT,YTXT,Q,'  '//QTXT)
      
      END SUBROUTINE ARROWTEXT
      
!###====================================================================
      SUBROUTINE PLOTDRAIN(QIN,QUIT,FX,FY,P,QTXT,B,H)
!###====================================================================
      IMPLICIT NONE
      CHARACTER(LEN=*),INTENT(INOUT) :: QTXT
      REAL(KIND=DP_KIND),INTENT(IN) :: B,H,FX,FY,P,QIN,QUIT
      REAL(KIND=DP_KIND) :: XDRN,YDRN
      
      CALL IGRLINEWIDTH(1)
      CALL IGRFILLPATTERN(0,0,0)
      XDRN=FX*B
      YDRN=FY*H
      IF(QUIT+QIN.LT.0) THEN
         CALL IGRFILLPATTERN(4,4,4)
         CALL IGRCOLOURN(WRGB(255,100,100))
         CALL DBL_IGRCIRCLE(XDRN,YDRN,P*B)
         CALL IGRFILLPATTERN(0,0,0)
         CALL IGRCOLOURN(WRGB(0,0,0))
         CALL DBL_IGRCIRCLE(XDRN,YDRN,P*B)
         CALL ARROWTEXT(XDRN,YDRN,XDRN,YDRN+0.1*h,&
                        XDRN,YDRN+0.1*H,&
                        0,90.0D0,WRGB(255,0,0),ABS(QIN+QUIT),QTXT(1:LEN_TRIM(QTXT)))
      ENDIF
      IF(QIN+QUIT.GT.0) THEN
         CALL IGRFILLPATTERN(4,4,4)
         CALL IGRCOLOURN(WRGB(255,100,100))
         CALL DBL_IGRCIRCLE(XDRN,YDRN,P*B)
         CALL IGRFILLPATTERN(0,0,0)
         CALL IGRCOLOURN(WRGB(0,0,0))
         CALL DBL_IGRCIRCLE(XDRN,YDRN,P*B)
         CALL ARROWTEXT(XDRN,YDRN+0.1*H,XDRN,YDRN,&
                        XDRN,YDRN+0.1*H,&
                        0,90.0D0,WRGB(0,0,255),ABS(QIN+QUIT),QTXT(1:LEN_TRIM(QTXT)))
      ENDIF                                     
      
      END SUBROUTINE PLOTDRAIN

!###====================================================================
      SUBROUTINE DVOLUME(QIN,QUIT,F,QTXT,B,H)
!###====================================================================
      IMPLICIT NONE
      REAL(KIND=DP_KIND),INTENT(IN) :: QIN,QUIT,B,H,F
      CHARACTER(LEN=*),INTENT(INOUT) :: QTXT
      REAL(KIND=DP_KIND) :: X,Y
      
      IF(QIN.NE.0.OR.QUIT.NE.0) THEN
      	 X=0.33*B
      	 Y=F*H
         CALL IGRFILLPATTERN(4,4,4)
         IF(QIN+QUIT.LT.0) THEN
             CALL IGRCOLOURN(WRGB(255,0,0))
         ELSE
             CALL IGRCOLOURN(WRGB(0,0,255))
         ENDIF
         CALL IGRELLIPSE(REAL(X,4),REAL(Y,4),REAL(B,4)/100,1.4)
         CALL DBL_IGRMOVETO(X,Y)
         CALL DBL_IGRLINETO(X+0.1*B,Y)
         CALL IGRCOLOURN(WRGB(0,0,0))
         CALL DBL_WGRTEXTORIENTATION(IALIGN=0,ANGLE=0.0D0,IDIR=0)
         CALL OUTVALUE(X,Y+0.025D0*H,ABS(QIN+QUIT),QTXT(1:LEN_TRIM(QTXT)))
      ENDIF
      
      END SUBROUTINE DVOLUME

!###====================================================================
      SUBROUTINE PLOTRIV(QIN,QUIT,F,ICOL,QTXT,B,H)                                                                     
!###====================================================================
      IMPLICIT NONE
      REAL(KIND=DP_KIND),INTENT(IN) :: QIN,QUIT,B,H,F
      INTEGER,INTENT(IN) :: ICOL
      CHARACTER(LEN=*),INTENT(INOUT) :: QTXT                                                                                    
      REAL(KIND=DP_KIND) :: XTRAP1,YTRAP1,XTRAP2,YTRAP2,XL1,XL2

      IF(QIN.NE.0.OR.QUIT.NE.0) THEN                                                                        
!        PLOT RIVER                                                                                         
         XTRAP1=F*B; YTRAP1=0.75*H                                                                                       
         XL1=0.025D0*B; XL2=3*XL1
         XTRAP2=XTRAP1+2*XL1
         YTRAP2=0.8*H                                                                                       
         CALL IGRFILLPATTERN(4,4,4)                                                                         
         CALL IGRCOLOURN(ICOL)                                                                              
         CALL IGRTRAPEZIUM(REAL(XTRAP1,4),REAL(YTRAP1,4),REAL(XTRAP2,4),REAL(YTRAP2,4),REAL(XL1,4),REAL(XL2,4),2)                                           
         CALL IGRFILLPATTERN(0,0,0)                                                                         
         CALL IGRCOLOURN(WRGB(0,0,0))                                                                       
         CALL IGRTRAPEZIUM(REAL(XTRAP1,4),REAL(YTRAP1,4),REAL(XTRAP2,4),REAL(YTRAP2,4),REAL(XL1,4),REAL(XL2,4),2)                                           
         IF(QUIT.NE.0) THEN                                                                                 
            CALL ARROWTEXT(XTRAP1,(YTRAP2+YTRAP1)/2,XTRAP1,(YTRAP2+YTRAP1)/2+0.1*H,&
                           XTRAP1,(YTRAP2+YTRAP1)/2+0.1*H,&
                           0,90.0D0,WRGB(255,0,0),ABS(QUIT),QTXT(1:LEN_TRIM(QTXT)))
         ENDIF                                                                                              
         IF(QIN.NE.0) THEN                                                                                  
            CALL ARROWTEXT(XTRAP1+XL1,(YTRAP2+YTRAP1)/2+0.1*H,XTRAP1+XL1,(YTRAP2+YTRAP1)/2,&
                           XTRAP1+XL1,(YTRAP2+YTRAP1)/2+0.1*H,&
                           0,90.0D0,WRGB(0,0,255),QIN,QTXT(1:LEN_TRIM(QTXT)))
         ENDIF                                                                                              
      ENDIF                                                                                                 

      END SUBROUTINE PLOTRIV
      
!###====================================================================
      SUBROUTINE PLOTWEL(QIN,QUIT,F,QTXT,B,H)
!###====================================================================
      IMPLICIT NONE
      REAL(KIND=DP_KIND),INTENT(IN) :: QIN,QUIT,B,H,F
      CHARACTER(LEN=*),INTENT(INOUT) :: QTXT
      REAL(KIND=DP_KIND) :: XWEL

      IF(QIN.NE.0.OR.QUIT.NE.0) THEN
!        PLOT WEL
         XWEL=F*B
         CALL IGRFILLPATTERN(4,4,4)
         CALL IGRCOLOURN(WRGB(255,100,255))
         CALL DBL_IGRRECTANGLE(XWEL-0.01D0*B,0.3D0*H,XWEL+0.01D0*B,0.6D0*H)
         CALL IGRFILLPATTERN(0,0,0)
         CALL IGRCOLOURN(WRGB(0,0,0))
         CALL DBL_IGRRECTANGLE(XWEL-0.01D0*B,0.3D0*H,XWEL+0.01D0*B,0.6D0*H)
         CALL DBL_IGRRECTANGLE(XWEL-0.06D0*B,0.3D0*H,XWEL+0.06D0*B,0.6D0*H)
         CALL DBL_IGRRECTANGLE(XWEL-0.02D0*B,0.3D0*H,XWEL+0.02D0*B,0.6D0*H)
         IF(QUIT.NE.0) THEN
            CALL ARROWTEXT(XWEL-0.03D0*B,0.5D0*H,XWEL-0.03D0*B,0.6D0*H,&
                           XWEL-0.03D0*B,0.5D0*H,&
                           2,90.0D0,WRGB(255,0,0),ABS(QUIT),QTXT(1:LEN_TRIM(QTXT)))
         ENDIF
         IF(QIN.NE.0) THEN
            CALL ARROWTEXT(XWEL+0.03D0*B,0.6D0*H,XWEL+0.03D0*B,0.5D0*H,&
                           XWEL+0.03D0*B,0.5D0*H,&
                           2,90.0D0,WRGB(0,0,255),QIN,QTXT(1:LEN_TRIM(QTXT)))
         ENDIF
      ENDIF
      
      END SUBROUTINE PLOTWEL

!###====================================================================
      SUBROUTINE PLOTREG(QIN,QUIT,F,QTXT,B,H)
!###====================================================================
      IMPLICIT NONE
      REAL(KIND=DP_KIND),INTENT(IN) :: QIN,QUIT,B,H,F
      CHARACTER(LEN=*),INTENT(INOUT) :: QTXT
      
      IF(QIN.NE.0)  CALL ARROWTEXT(0.1D0*B,F*H,0.2D0*B,F*H,&
                                   0.1D0*B,(F+0.025D0)*H,&
                                   0,0.0D0,WRGB(0,0,255),QIN,QTXT(1:LEN_TRIM(QTXT)))

      IF(QUIT.NE.0) CALL ARROWTEXT(0.8D0*B,F*H,0.9D0*B,F*H,&
                                   0.8D0*B,(F+0.025D0)*H,&
                                   0,0.0D0,WRGB(255,0,0),ABS(QUIT),QTXT(1:LEN_TRIM(QTXT)))

      END SUBROUTINE PLOTREG

!###====================================================================
      SUBROUTINE VTERM2(QIN,QUIT,FX1,FX2,FY,QTXT,B,H,IPOS,ITOP)             
!###====================================================================
      IMPLICIT NONE                                              
      REAL(KIND=DP_KIND),INTENT(IN) :: QIN,QUIT,B,H,FX1,FX2,FY
      CHARACTER(LEN=*),INTENT(INOUT) ::  QTXT                                          
      INTEGER,INTENT(IN) :: IPOS,ITOP
      
      IF(ITOP.EQ.1) THEN
         IF(IPOS.EQ.2) THEN
            IF(QIN.NE.0) CALL ARROWTEXT(FX1*B,(FY+0.1D0)*H,FX1*B,FY*H,&
                                        FX1*B,(FY-0.01D0)*H,&
                                        IPOS,90.0D0,WRGB(0,0,255),QIN,QTXT(1:LEN_TRIM(QTXT)))
            IF(QUIT.NE.0) CALL ARROWTEXT(FX2*B,FY*H,FX2*B,(FY+0.1)*H,&
                                         FX2*B,(FY-0.01D0)*H,&
                                         IPOS,90.0D0,WRGB(255,0,0),ABS(QUIT),QTXT(1:LEN_TRIM(QTXT)))
         ELSE IF(IPOS.EQ.0) THEN
            IF(QIN.NE.0) CALL ARROWTEXT(FX1*B,(FY+0.1D0)*H,FX1*B,FY*H,&
                                        FX1*B,(FY+0.11D0)*H,&
                                        IPOS,90.0D0,WRGB(0,0,255),QIN,QTXT(1:LEN_TRIM(QTXT)))
            IF(QUIT.NE.0) CALL ARROWTEXT(FX2*B,FY*H,FX2*B,(FY+0.1D0)*H,&
                                         FX2*B,(FY+0.11D0)*H,&
                                        IPOS,90.0D0,WRGB(255,0,0),ABS(QUIT),QTXT(1:LEN_TRIM(QTXT)))
         ENDIF
      ELSE
         IF(IPOS.EQ.2) THEN
            IF(QIN.NE.0) CALL ARROWTEXT(FX1*B,FY*H,FX1*B,(FY+0.1)*H,&
                                        FX1*B,(FY-0.01D0)*H,&
                                        IPOS,90.0D0,WRGB(0,0,255),QIN,QTXT(1:LEN_TRIM(QTXT)))
            IF(QUIT.NE.0) CALL ARROWTEXT(FX2*B,(FY+0.1D0)*H,FX2*B,FY*H,&
                                         FX2*B,(FY-0.01D0)*H,&
                                         IPOS,90.0D0,WRGB(255,0,0),ABS(QUIT),QTXT(1:LEN_TRIM(QTXT)))
         ELSE IF(IPOS.EQ.0) THEN
            IF(QIN.NE.0) CALL ARROWTEXT(FX1*B,FY*H,FX1*B,(FY+0.1D0)*H,&
                                        FX1*B,(FY+0.09D0)*H,&
                                        IPOS,90.0D0,WRGB(0,0,255),QIN,QTXT(1:LEN_TRIM(QTXT)))
            IF(QUIT.NE.0) CALL ARROWTEXT(FX2*B,(FY+0.1D0)*H,FX2*B,FY*H,&
                                         FX2*B,(FY+0.09D0)*H,&
                                        IPOS,90.0D0,WRGB(255,0,0),ABS(QUIT),QTXT(1:LEN_TRIM(QTXT)))
         ELSE IF(IPOS.EQ.3) THEN
            IF(QIN.NE.0) CALL ARROWTEXT(FX1*B,FY*H,FX1*B,(FY+0.1D0)*H,&
                                        (FX1-0.022D0)*B,(FY+0.12D0)*H,&
                                        IPOS,90.0D0,WRGB(0,0,255),QIN,QTXT(1:LEN_TRIM(QTXT)))
            IF(QUIT.NE.0) CALL ARROWTEXT(FX2*B,(FY+0.1)*H,FX2*B,FY*H,&
                                        (FX2+0.022D0)*B,(FY+0.12D0)*H,&
                                        IPOS,90.0D0,WRGB(255,0,0),ABS(QUIT),QTXT(1:LEN_TRIM(QTXT)))

         ENDIF
      ENDIF
      
      END SUBROUTINE VTERM2

!###====================================================================
      SUBROUTINE OUTVALUE(X,Y,VALUE,ITEM)
!###====================================================================
      IMPLICIT NONE
      REAL(KIND=DP_KIND),INTENT(IN) :: X,Y,VALUE
      CHARACTER(LEN=*),INTENT(IN) :: ITEM
      INTEGER :: ILAST
      CHARACTER(LEN=15) :: FMT
      CHARACTER(LEN=10) :: STR
    
!      FMT=MAKEFMT(VALUE)//'       '
      FMT=UTL_GETFORMAT(VALUE)
      
      CALL IGRCOLOURN(WRGB(0,0,0))
      IF(INDEX(FMT,'I10').GT.0) THEN
         WRITE(STR,FMT) INT(VALUE)
         CALL LASTCHAR(STR,' ',ILAST); ILAST=MAX0(ILAST,1)
         CALL DBL_WGRTEXTSTRING(X,Y,ITEM//'='//STR(ILAST:10) )
      ELSE
         WRITE(STR,FMT) VALUE
         CALL LASTCHAR(STR,' ',ILAST); ILAST=MAX0(ILAST,1)
         CALL DBL_WGRTEXTSTRING(X,Y,ITEM//'='//STR(ILAST:10))
      ENDIF

      END SUBROUTINE OUTVALUE

!###====================================================================
      SUBROUTINE CSIZE(CS,CW,CH,IPLOT)
!###====================================================================
      IMPLICIT NONE
      REAL(KIND=DP_KIND),INTENT(INOUT) :: CW,CH
      INTEGER,INTENT(IN) :: IPLOT
      REAL(KIND=DP_KIND),INTENT(IN) :: CS
      REAL(KIND=DP_KIND) :: DX,DY

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
      REAL(KIND=DP_KIND),INTENT(IN) :: F,H,Y,X
      REAL(KIND=DP_KIND) :: R,DX,DY,R0,X0,Y0    
      INTEGER :: I,IGRE,IGRE0,K

      IGRE0=50+(IRANDOMNUMBER(1))*100
      R0=H/3
      X0=X
      Y0=(Y+H-R0)
      CALL IGRCOLOURMODEL(24)
      CALL IGRFILLPATTERN(4,4,4)
      CALL IGRCOLOURN(WRGB(100,0,0))
      CALL DBL_IGRRECTANGLE(X0-R0/10.0D0,Y,X0+R0/10.0D0,Y0)
      CALL IGRFILLPATTERN(0,0,0)
      DO I=1,1000
         R=(IRANDOMNUMBER(1)-0.5D0)*2.0D0*R0/2.0D0
         IGRE=10+(IRANDOMNUMBER(1))*190
         CALL IGRCOLOURN(WRGB(0,IGRE,0))
         DX=(IRANDOMNUMBER(1)-0.5D0)*2.0D0*R0
         DY=(IRANDOMNUMBER(1)-0.5D0)*2.0D0*R0
         IF(DX**2.0D0+DY**2.0D0.LT.R0**2.0D0) THEN
             CALL IGRELLIPSE(REAL(X0-DX,4),REAL(Y0-DY,4),ABS(REAL(R,4)),REAL(F,4))
             CALL DBL_IGRCIRCLE(X0-DX,Y0-DY,ABS(R))
         ENDIF
      ENDDO

!     APPLES IN THE TREE
      CALL IGRFILLPATTERN(4,4,4)
      DO I=1,20
         DX=(IRANDOMNUMBER(1)-0.5D0)*2.0D0*R0
         DY=(IRANDOMNUMBER(1)-0.5D0)*2.0D0*R0
         IGRE=125+(IRANDOMNUMBER(1))*125
         CALL IGRCOLOURN(WRGB(255,IGRE,0))
         CALL DBL_IGRCIRCLE(X0-DX,Y0-DY,H/40)
      ENDDO
     
      CALL IGRCOLOURN(WRGB(0,0,0))
      DO K=1,20
         DX=(IRANDOMNUMBER(1)-0.5D0)*2.0D0*R0
         CALL DBL_IGRJOIN(X,Y,X-DX,Y-IRANDOMNUMBER(1)*R0)
      ENDDO
      DO K=1,20
         DX=(IRANDOMNUMBER(1)-0.5D0)*2.0D0*R0*0.5D0
         CALL DBL_IGRJOIN(X,Y,X-DX,Y-IRANDOMNUMBER(1)*R0)
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
      SUBROUTINE PLOTSUBREGIO(II,JJ,QIN,QUIT,REGNAME,JPOL,NPOL,ICOLPLG,IPLG,DH,B,H,ifit)
!###====================================================================
      IMPLICIT NONE
      INTEGER,INTENT(IN),DIMENSION(NPOL) :: ICOLPLG,IPLG
      INTEGER,INTENT(IN) :: JPOL,NPOL
      REAL(KIND=DP_KIND),INTENT(IN) :: QIN,QUIT,B,H,DH
      INTEGER :: J,II,JJ,IPOS,ILEN,IFIT
      CHARACTER(LEN=256) STR
      CHARACTER(LEN=10) STRDUM
      CHARACTER(LEN=256) :: REGNAME

      STR=' '; STRDUM=' '; IPOS=1; WRITE(STRDUM(1:7),'(I7)') IPLG(JPOL)
      DO J=1,7
         IF(STRDUM(J:J).EQ.' ') IPOS=J+1
      ENDDO

      ILEN=MIN0(1+(7-IPOS+1)+5+LEN_TRIM(REGNAME)+1,256)
      STR(1:ILEN)='('//STRDUM(IPOS:7)//') - ('//REGNAME(1:LEN_TRIM(REGNAME))//')'

      IF(QIN.GT.0) THEN
         II=II+1
         CALL ARROWTEXT(0.15D0*B,0.65D0*H-II*DH,0.2D0*B,0.65D0*H-II*DH,&
                       0.15D0*B,0.65D0*H+DH/2.0D0-II*DH,&
                       0,0.0D0,WRGB(0,0,255),QIN,STR(1:ILEN))
         IF(IFIT.EQ.0) THEN
            CALL PASTEL(ICOLPLG(JPOL),0.6D0); CALL DBL_IGRCIRCLE(0.14D0*B,0.65D0*H-II*DH,0.06D0*B)
         ELSE
            CALL IGRCOLOURN(WRGB(255,125,0)); CALL DBL_IGRCIRCLE(0.14D0*B,0.65D0*H-II*DH,0.06D0*B)
         ENDIF
         CALL IGRCOLOURN(WRGB(255,125,0)); CALL DBL_IGRCIRCLE(0.21D0*B,0.65D0*H-II*DH,0.06D0*B)

         CALL IGRFILLPATTERN(0,0,0)
         CALL IGRLINETYPE(0)
         CALL IGRCOLOURN(WRGB(0,0,0))
         CALL DBL_IGRCIRCLE(0.14D0*B,0.65D0*H-II*DH,0.06D0*B)
         CALL DBL_IGRCIRCLE(0.21D0*B,0.65D0*H-II*DH,0.06D0*B)
      ENDIF

      STR=' '; STRDUM=' '; IPOS=1; WRITE(STRDUM(1:7),'(I7)') IPLG(JPOL)
      DO J=1,7
         IF(STRDUM(J:J).EQ.' ') IPOS=J+1
      ENDDO

      ILEN=MIN0(1+LEN_TRIM(REGNAME)+5+(7-IPOS+1)+1,256)
      STR(1:ILEN)='('//REGNAME(1:LEN_TRIM(REGNAME))//') - ('//STRDUM(IPOS:7)//')'
      
      IF(QUIT.LT.0) THEN
         JJ=JJ+1
         CALL ARROWTEXT(0.78D0*B,0.65D0*H-JJ*DH,0.83D0*B,0.65D0*H-JJ*DH,&
                        0.78D0*B,0.65D0*H+DH/2.0D0-JJ*DH,&
                        0,0.0D0,WRGB(255,0,0),ABS(QUIT),STR(1:ILEN))

         CALL IGRCOLOURN(WRGB(255,125,0)); CALL DBL_IGRCIRCLE(0.77D0*B,0.65D0*H-JJ*DH,0.06D0*B)
         IF(IFIT.EQ.0) THEN
            CALL PASTEL(ICOLPLG(JPOL),0.6D0); CALL DBL_IGRCIRCLE(0.84D0*B,0.65D0*H-JJ*DH,0.06D0*B)
         ELSE
            CALL IGRCOLOURN(WRGb(255,125,0)); CALL DBL_IGRCIRCLE(0.84D0*B,0.65D0*H-JJ*DH,0.06D0*B)
         endif

         CALL IGRFILLPATTERN(0,0,0)
         CALL IGRCOLOURN(WRGB(0,0,0))
         CALL IGRLINETYPE(0)
         CALL DBL_IGRCIRCLE(0.84D0*B,0.65D0*H-JJ*DH,0.06D0*B)
         CALL DBL_IGRCIRCLE(0.77D0*B,0.65D0*H-JJ*DH,0.06D0*B)
      ENDIF
      
      END SUBROUTINE PLOTSUBREGIO

!###====================================================================
      SUBROUTINE PLOTIDF(IIPOL,NPOL,IPLG,ICOLPLG,IPLOT,LOCAL,AREA,IDF,LSUM,SUMNR)
!###====================================================================
      IMPLICIT NONE
      INTEGER,INTENT(IN),DIMENSION(:) :: IIPOL
      REAL(KIND=DP_KIND),INTENT(OUT) :: AREA
      INTEGER,DIMENSION(NPOL),INTENT(IN) :: IPLG,ICOLPLG
      INTEGER,INTENT(IN) :: IPLOT,SUMNR
      TYPE (IDFOBJ),INTENT(IN) :: IDF
      INTEGER :: I,J,IPOL,KPOL,NPOL,K,II,ISEL
      REAL(KIND=DP_KIND) :: X1PLOT,X0PLOT,Y1PLOT,Y0PLOT
      REAL(KIND=DP_KIND) :: FAREAX1,FAREAX2,FAREAY1,FAREAY2
      LOGICAL :: LOCAL,LSUM

!     PLOT REGIONS
      FAREAX1=0.68D0
      FAREAX2=0.85D0
      AREA=0.0D0

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
                  X0PLOT=MIN(X0PLOT,IDF%SX(I-1)); X1PLOT=MAX(X1PLOT,IDF%SX(I)) 
                  Y0PLOT=MIN(Y0PLOT,IDF%SY(J-1)); Y1PLOT=MAX(Y1PLOT,IDF%SY(J)) 
!                  X0PLOT=AMIN1(X0PLOT,IDF%SX(I-1)); X1PLOT=AMAX1(X1PLOT,IDF%SX(I)) 
!                  Y0PLOT=AMIN1(Y0PLOT,IDF%SY(J-1)); Y1PLOT=AMAX1(Y1PLOT,IDF%SY(J)) 
               ENDIF
              ENDDO
           ELSE
               IF(ABS(NINT(IDF%X(I,J))).EQ.SUMNR) THEN
                 X0PLOT=MIN(X0PLOT,IDF%SX(I-1)); X1PLOT=MAX(X1PLOT,IDF%SX(I)) 
                 Y0PLOT=MIN(Y0PLOT,IDF%SY(J-1)); Y1PLOT=MAX(Y1PLOT,IDF%SY(J)) 
!                 X0PLOT=AMIN1(X0PLOT,IDF%SX(I-1)); X1PLOT=AMAX1(X1PLOT,IDF%SX(I)) 
!                 Y0PLOT=AMIN1(Y0PLOT,IDF%SY(J-1)); Y1PLOT=AMAX1(Y1PLOT,IDF%SY(J)) 
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

      FAREAY1=0.01D0
      FAREAY2=FAREAY1+((FAREAX2-FAREAX1)*REAL(WINFOBITMAP(IPLOT,1))/REAL(WINFOBITMAP(IPLOT,2)))*(Y1PLOT-Y0PLOT)/(X1PLOT-X0PLOT)

      CALL DBL_IGRAREA(FAREAX1,FAREAY1,FAREAX2,FAREAY2)
      CALL DBL_IGRUNITS(X0PLOT,Y0PLOT,X1PLOT,Y1PLOT)
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
                IF(ABS(NINT(IDF%X(I,J))).EQ.IPLG(K)) CALL PASTEL(ICOLPLG(K),0.6D0)
             ENDDO
          ENDIF
          IF(IDF%X(I,J).NE.IDF%NODATA) CALL DBL_IGRRECTANGLE(IDF%SX(I-1),IDF%SY(J-1),IDF%SX(I),IDF%SY(J))
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
                       IF(ABS(NINT(IDF%X(I-1,J))).NE.ABS(NINT(IDF%X(I,J)))) CALL DBL_IGRJOIN(IDF%SX(I-1),IDF%SY(J-1),IDF%SX(I-1),IDF%SY(J))
                       IF(ABS(NINT(IDF%X(I+1,J))).NE.ABS(NINT(IDF%X(I,J)))) CALL DBL_IGRJOIN(IDF%SX(I),  IDF%SY(J-1),IDF%SX(I),  IDF%SY(J))
                   ENDIF                  
                   IF(J.GT.1.AND.J.LT.IDF%NROW.AND.ABS(NINT(IDF%X(I,J))).EQ.IPLG(IPOL)) THEN
                       IF(ABS(NINT(IDF%X(I,J-1))).NE.ABS(NINT(IDF%X(I,J)))) CALL DBL_IGRJOIN(IDF%SX(I-1),IDF%SY(J-1),IDF%SX(I),IDF%SY(J-1))
                       IF(ABS(NINT(IDF%X(I,J+1))).NE.ABS(NINT(IDF%X(I,J)))) CALL DBL_IGRJOIN(IDF%SX(I-1),IDF%SY(J),  IDF%SX(I),IDF%SY(J))
                   ENDIF
                   IF(I.EQ.1.AND.ABS(NINT(IDF%X(I,J))).EQ.IPLG(IPOL)) CALL DBL_IGRJOIN(IDF%SX(I-1),IDF%SY(J-1),IDF%SX(I-1),IDF%SY(J))
                   IF(I.EQ.IDF%NCOL.AND.ABS(NINT(IDF%X(I,J))).EQ.IPLG(IPOL)) CALL DBL_IGRJOIN(IDF%SX(I),  IDF%SY(J-1),IDF%SX(I),  IDF%SY(J))
                   IF(J.EQ.1.AND.ABS(NINT(IDF%X(I,J))).EQ.IPLG(IPOL)) CALL DBL_IGRJOIN(IDF%SX(I-1),IDF%SY(J-1),IDF%SX(I),  IDF%SY(J-1))
                   IF(J.EQ.IDF%NROW.AND.ABS(NINT(IDF%X(I,J))).EQ.IPLG(IPOL)) CALL DBL_IGRJOIN(IDF%SX(I-1),IDF%SY(J),  IDF%SX(I),  IDF%SY(J))
                ENDDO
            ELSE
                IF(I.GT.1.AND.I.LT.IDF%NCOL.AND.ABS(NINT(IDF%X(I,J))).EQ.SUMNR) THEN
                    IF(ABS(NINT(IDF%X(I-1,J))).NE.ABS(NINT(IDF%X(I,J)))) CALL DBL_IGRJOIN(IDF%SX(I-1),IDF%SY(J-1),IDF%SX(I-1),IDF%SY(J))
                    IF(ABS(NINT(IDF%X(I+1,J))).NE.ABS(NINT(IDF%X(I,J)))) CALL DBL_IGRJOIN(IDF%SX(I),  IDF%SY(J-1),IDF%SX(I),  IDF%SY(J))
                ENDIF                  
                IF(J.GT.1.AND.J.LT.IDF%NROW.AND.ABS(NINT(IDF%X(I,J))).EQ.SUMNR) THEN
                    IF(ABS(NINT(IDF%X(I,J-1))).NE.ABS(NINT(IDF%X(I,J)))) CALL DBL_IGRJOIN(IDF%SX(I-1),IDF%SY(J-1),IDF%SX(I),IDF%SY(J-1))
                    IF(ABS(NINT(IDF%X(I,J+1))).NE.ABS(NINT(IDF%X(I,J)))) CALL DBL_IGRJOIN(IDF%SX(I-1),IDF%SY(J),  IDF%SX(I),IDF%SY(J))
                ENDIF
                IF(I.EQ.1.AND.ABS(NINT(IDF%X(I,J))).EQ.SUMNR) CALL DBL_IGRJOIN(IDF%SX(I-1),IDF%SY(J-1),IDF%SX(I-1),IDF%SY(J))
                IF(I.EQ.IDF%NCOL.AND.ABS(NINT(IDF%X(I,J))).EQ.SUMNR) CALL DBL_IGRJOIN(IDF%SX(I),  IDF%SY(J-1),IDF%SX(I),  IDF%SY(J))
                IF(J.EQ.1.AND.ABS(NINT(IDF%X(I,J))).EQ.SUMNR) CALL DBL_IGRJOIN(IDF%SX(I-1),IDF%SY(J-1),IDF%SX(I),  IDF%SY(J-1))
                IF(J.EQ.IDF%NROW.AND.ABS(NINT(IDF%X(I,J))).EQ.SUMNR) CALL DBL_IGRJOIN(IDF%SX(I-1),IDF%SY(J),  IDF%SX(I),  IDF%SY(J))
            ENDIF
         ENDDO
      ENDDO

      CALL IGRCOLOURN(WRGB(0,0,0))
      CALL IGRFILLPATTERN(0,0,0)
      CALL DBL_IGRRECTANGLE(X0PLOT,Y0PLOT,X1PLOT,Y1PLOT)
      
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
      REAL(KIND=DP_KIND) :: FADE
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
      REAL(KIND=DP_KIND),INTENT(IN) :: VALUE
      CHARACTER(LEN=8) :: MAKEFMT

      MAKEFMT='(F10.1) '
      IF(ABS(VALUE).GT.100000)                        MAKEFMT='(G10.4) '
      IF(ABS(VALUE).GT.10  .AND.ABS(VALUE).LE.100000) MAKEFMT='(F10.0) '
      IF(ABS(VALUE).GT.1   .AND.ABS(VALUE).LE.10   )  MAKEFMT='(F10.1) '
      IF(ABS(VALUE).GT.0.01D0.AND.ABS(VALUE).LE.1)      MAKEFMT='(F10.2) '
      IF(ABS(VALUE).GT.0   .AND.ABS(VALUE).LE.0.01D0)   MAKEFMT='(G10.4) '

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
      REAL(KIND=DP_KIND), INTENT(INOUT) :: X0PLOT,Y0PLOT,X1PLOT,Y1PLOT
      REAL(KIND=DP_KIND) :: DXWIN,DYWIN

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
      X0PLOT=X0PLOT-0.05D0*DXWIN; X1PLOT=X1PLOT+0.05D0*DXWIN;      
      Y0PLOT=Y0PLOT-0.05D0*DYWIN; Y1PLOT=Y1PLOT+0.05D0*DYWIN;      

      END SUBROUTINE MAKESQUAREWINDOW

END MODULE MOD_WBAL_GRAPHICS


