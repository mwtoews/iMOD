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

!###====================================================================
MODULE MOD_METASWAP
!###====================================================================
USE IMOD_UTL, ONLY : OS,MSWPMV
USE IMOD_UTL, ONLY : IMOD_UTL_PRINTTEXT,IMOD_UTL_ITOS,IMOD_UTL_RTOS
USE IMOD_UTL, ONLY : IMOD_UTL_OPENASC,IMOD_UTL_STRING,IMOD_UTL_FILENAME
USE IMOD_UTL, ONLY : IMOD_UTL_MODEL1CHECKFNAME,IMOD_UTL_CAP
USE IMOD_UTL, ONLY : IMOD_UTL_APPLYFCT_R,IMOD_UTL_APPLYFCT_I,GETUNIT,IMOD_UTL_CAPF,IMOD_UTL_SCALE1PDELRC
USE IMOD_UTL, ONLY : IMOD_UTL_POL1LOCATER
USE MOD_RF2MF, ONLY : IIDEBUG,IURUN,IACT,RESULTDIR,SIMBOX,SIMCSIZE,PCAP,MDIM,IDFTINY,IARMWP, &
                   ISAVE,CDATE,MMOD,PCAP,PPWT,LINE,NLINES,ISCEN,SIMCSIZE,USEBOX,IFULL
USE MOD_RF2MF_READ, ONLY: RF2MF_READ_IDF
USE rf2mf_module, ONLY: NLAY,NROW,NCOL,root, dxc, nam, idxcflux
USE MOD_RF2MF, ONLY: DELR,DELC
USE IDFMODULE
USE IMOD_IDF, ONLY: IDFWRITE_WRAPPER
IMPLICIT NONE

PRIVATE

INTEGER :: SIMGRO_NROW,SIMGRO_NCOL,SIMGRO_NLAY
TYPE SIMGRO_OBJ
 INTEGER :: IBOUND   !boundary condition
 INTEGER :: LGN      !landuse
 INTEGER :: METEO    !meteo-station
 INTEGER :: BER_LAAG !artificial recharge layer
 INTEGER :: BEREGEN  !artificial recharge
 INTEGER :: BODEM    !soil type
 REAL    :: BEREGEN_Q  !artificial recharge strength
 REAL    :: NOPP     !wetted-surface
 REAL    :: SOPP     !urban-surface
 REAL    :: RZ       !rootzone
 REAL    :: MV       !surface-level
 REAL    :: PWT_LEVEL !level for PWT (optional)
 REAL    :: COND      !conductivity
 REAL    :: MOISTURE  !moisture
 REAL    :: VXMU_SOPP !micro-storage capacity, sill of the runoff relationship
 REAL    :: VXMU_ROPP !micro-storage capacity, sill of the runoff relationship
 REAL    :: CRUNOFF_SOPP !runoff resistance (days)
 REAL    :: CRUNOFF_ROPP !runoff resistance (days)
 REAL    :: CRUNON_SOPP !runon resistance (days)
 REAL    :: CRUNON_ROPP !runon resistance (days)
 REAL    :: QINFBASIC_SOPP !infiltratie cap.
 REAL    :: QINFBASIC_ROPP
END TYPE SIMGRO_OBJ
TYPE(SIMGRO_OBJ),ALLOCATABLE,DIMENSION(:,:) :: SIMGRO         !SIMGRO(NCOL,NROW)%[VARIABELE]
INTEGER :: INDSB                                              !unit number for svat2swnr_roff.inp
INTEGER :: IAREA                                              !unit number for area_msw.inp
INTEGER :: ISELSVAT                                           !unit number for sel_svat_bda.inp
INTEGER :: ISCAP                                              !unit number for scap_msw.inp
INTEGER :: IGWMP                                              !unit number for gwmp_msw.inp
INTEGER :: IDXC                                               !unit number for MODFLOW DXC file
INTEGER :: IMODSIM                                            !unit number for mod-sim.txt
INTEGER :: IINFI                                              !unit number for infi_svat.inp
INTEGER :: IIDF                                               !unit number for idf_svat.inp
INTEGER :: IUSCL                                              !unit number for uscl_svat.inp

PUBLIC :: RF2MF_METASWAP
PUBLIC :: METASWAP_EXPORT4
PUBLIC :: METASWAP_PARASIM
PUBLIC :: METASWAP_METEGRID
PUBLIC :: METASWAP_METEGRID_INP
PUBLIC :: METASWAP_CHECK
PUBLIC :: storedxc
PUBLIC :: writedxc

CONTAINS

 !####====================================================================
 SUBROUTINE RF2MF_METASWAP(DXCFILE)
 !####====================================================================
 IMPLICIT NONE

 CHARACTER(LEN=*),INTENT(INOUT) :: DXCFILE

 !## dummy variables
 REAL,ALLOCATABLE,DIMENSION(:,:) :: BUFF
 INTEGER :: ICOL,IROW,SCLTYPE,II,I,ISMOOTH,ILAY,NIDF
 INTEGER,DIMENSION(:),ALLOCATABLE :: IOS
 REAL :: DX,DY,ARND
 REAL,DIMENSION(:),ALLOCATABLE :: FCT,IMP,CONSTANTE,NODATA
 INTEGER,DIMENSION(:),ALLOCATABLE :: IERROR
 INTEGER,ALLOCATABLE,DIMENSION(:) :: ITMP
 CHARACTER(LEN=256) :: FFNAME
 CHARACTER(LEN=512) :: CL
 CHARACTER(LEN=256),DIMENSION(:),ALLOCATABLE :: FNAME
 CHARACTER(LEN=256),DIMENSION(22) :: MSWP_IDFNAMES
 DATA MSWP_IDFNAMES/ &
       'BOUNDARY [-]', &  !1
       'LANDUSE [-]', &   !2
       'ROOTZONE [cm]', & !3
       'SOIL_FYSICAL_UNITS [-]', &  !4
       'METEOSTATION [-]', &  !5
       'SURFACE_ELEVATION [m+MSL]', &  !6
       'ARTIFICIAL_RECHARGE_OCCURENCE [-]', & !7 (idf)
       'ARTIFICIAL_RECHARGE_LAYER [-]', &  !8 (ipf/idf)
       'ARTIFICIAL_RECHARGE_CAPACITY [mm/day]', &  !9
       'WETTED_AREA [m2]', & !10
       'URBAN_AREA [m2]', & !11
       'PONDING_DEPTH_URBAN_AREA [m]', & !12
       'PONDING_DEPTH RURAL_AREA [m]', & !13
       'RUNOFF_RESISTANCE_URBAN_AREA [d]', & !14
       'RUNOFF_RESISTANCE_RURAL_AREA [d]', & !15
       'RUNON_RESISTANCE_URBAN_AREA [d]', & !16
       'RUNON_RESISTANCE_RURAL_AREA [d]', & !17
       'QINFBASIC_URBAN_AREA [m/d]', & !18
       'QINFBASIC_RURAL_AREA [m/d]', & !19
       'PERCHED_WATERTABLE_LEVEL [m+MSL]', & !20
       'SOIL_MOISTURE [FACTOR]', & !21
       'CONDUCTIVITY [FACTOR]'/ !22
 LOGICAL :: LEX,lop
 character(len=256) :: curwd, modwd, simwd
 logical :: ok

 IF(NLINES.EQ.0)RETURN

 IF(MMOD(PCAP).EQ.0)THEN; DO ICOL=1,NLINES; READ(IURUN,*); END DO; RETURN; ENDIF

 ! set output directories
 call osd_getcwd(curwd)
 if (len_trim(root%submodel).gt.0) then
    write(modwd,'(4a)') trim(root%resultdir),'\',trim(root%submodel),'\mf2005_tmp\'
    write(simwd,'(4a)') trim(root%resultdir),'\',trim(root%submodel),'\metaswap\'
 else
    write(modwd,'(4a)') trim(root%resultdir),'\mf2005_tmp\'
    write(simwd,'(4a)') trim(root%resultdir),'\metaswap\'
 end if
 call osd_s_filename(modwd)
 call osd_s_filename(simwd)
 call imod_utl_createdir(modwd)
 call imod_utl_createdir(simwd)

 write(dxcfile,'(3a)') trim(modwd), trim(root%modelname), '.dxc'
 call imod_utl_s_cap(dxcfile,'l')

 NIDF=22 !; IF(IARMWP.EQ.1)NIDF=21
 IF(NLINES.LT.NIDF)CALL IMOD_UTL_PRINTTEXT('MetaSwap needs '//TRIM(IMOD_UTL_ITOS(NIDF))//' files, now reading is '//TRIM(IMOD_UTL_ITOS(NLINES)),2)

 CALL IMOD_UTL_PRINTTEXT('',0); CALL IMOD_UTL_PRINTTEXT('Constructing MetaSwap input files ...',0); CALL IMOD_UTL_PRINTTEXT('',0)

 ALLOCATE(FCT(NIDF),IMP(NIDF),CONSTANTE(NIDF),NODATA(NIDF),FNAME(NIDF),IERROR(NIDF),IOS(NIDF))
 ISAVE=1; CDATE='MSW'

 !## allocate memory
 IF(ALLOCATED(BUFF))DEALLOCATE(BUFF); IF(ALLOCATED(ITMP))DEALLOCATE(ITMP); IF(ALLOCATED(SIMGRO))DEALLOCATE(SIMGRO)
 ALLOCATE(BUFF(NCOL,NROW),ITMP(NCOL)); ALLOCATE(SIMGRO(NCOL,NROW))

 !## initialize unit numbers
 INDSB=0; IAREA=0; ISELSVAT=0; IGWMP=0; IMODSIM=0; ISCAP=0; IINFI=0; IIDF =0

 !## open indsb
 write(FFNAME,'(2a)') trim(simwd), 'svat2swnr_roff.inp'
 CALL IMOD_UTL_OPENASC(INDSB,FFNAME,'W')
 !## open iarea
 write(FFNAME,'(2a)') trim(simwd), 'area_svat.inp'
 CALL IMOD_UTL_OPENASC(IAREA,FFNAME,'W')
 !## open iscap
 write(FFNAME,'(2a)') trim(simwd), 'scap_svat.inp'
 CALL IMOD_UTL_OPENASC(ISCAP,FFNAME,'W')
 !## open igwmp
 write(FFNAME,'(2a)') trim(simwd), 'mod2svat.inp'
 CALL IMOD_UTL_OPENASC(IGWMP,FFNAME,'W')
 !## open MODFLOW dxc file
 CALL IMOD_UTL_OPENASC(IDXC,DXCFILE,'W')
 !## open MOD-SIM.TXT
 write(FFNAME,'(2a)') trim(simwd), 'MOD-SIM.TXT'
 CALL IMOD_UTL_OPENASC(IMODSIM,FFNAME,'W')
 !## open iselsvat
 write(FFNAME,'(2a)') trim(simwd), 'sel_svat_bda.inp'
 CALL IMOD_UTL_OPENASC(ISELSVAT,FFNAME,'W')
 !## open infi_svat.inp
 write(FFNAME,'(2a)') trim(simwd), 'infi_svat.inp'
 IINFI=GETUNIT()
 OPEN (IINFI,FILE=FFNAME,STATUS='UNKNOWN',CARRIAGECONTROL='LIST')
 !## open idf_svat.inp
 write(FFNAME,'(2a)') trim(simwd), 'idf_svat.inp'
 CALL IMOD_UTL_OPENASC(IIDF,FFNAME,'W')
 !## open uscl_svat.inp
 write(FFNAME,'(2a)') trim(simwd), 'uscl_svat.inp'
 CALL IMOD_UTL_OPENASC(IUSCL,FFNAME,'W')

 !## open all files
 IOS=0; DO II=1,NIDF-(IARMWP*1)
  !## correct for artificial recharge from ipf file
  I=II; IF(IARMWP.EQ.1.AND.II.GT.8)I=II+1
  READ(IURUN,'(A256)') LINE
  CALL IMOD_UTL_PRINTTEXT('',0); CALL IMOD_UTL_PRINTTEXT('Allocating '//TRIM(MSWP_IDFNAMES(I)),0)
  CALL RF2MF_READ_IDF(LINE,FCT(I),IMP(I),ILAY,CONSTANTE(I),FNAME(I),IOS(I),0,PCAP) !,3)
 ENDDO

 DO II=1,NIDF-(IARMWP*1)  !SIZE(MSWP_IDFNAMES)
  !## skip ipf file
  IF(IARMWP.EQ.1.AND.II.EQ.8)CYCLE
  !## correct for artificial recharge from ipf file
  I=II; IF(IARMWP.EQ.1.AND.II.GT.8)I=II+1
  SELECT CASE (I)
   CASE (1);             NODATA(I)=-999.99; SCLTYPE=1; ISMOOTH=0
   CASE (2:5,7:9);       NODATA(I)=-999.99; SCLTYPE=7; ISMOOTH=0
   CASE (6,12,13,20);    NODATA(I)=-999.99; SCLTYPE=2; ISMOOTH=1
   CASE (21,22);         NODATA(I)=-999.99; SCLTYPE=2; ISMOOTH=0
   CASE (18,19);         NODATA(I)=-999.99; SCLTYPE=6; ISMOOTH=0 !## scaling m/d -> reciprook -> m/d
   CASE (14:17);         NODATA(I)=-999.99; SCLTYPE=6; ISMOOTH=0
   CASE (10,11);         NODATA(I)=-999.99; SCLTYPE=5; ISMOOTH=0
  END SELECT
  !## process data
  IF(IOS(I).EQ.0)THEN
   BUFF=CONSTANTE(I); ! NODATA(I)=0.0
  ELSE
   if (.not.idfread(idfc,fname(i),0)) CALL IMOD_UTL_PRINTTEXT('idfread',2)
   call idfnullify(idfm)
   idfm%ieq=0
   idfm%dx=simcsize
   idfm%dy=simcsize
   idfm%ncol=ncol
   idfm%nrow=nrow
   idfm%xmin = simbox(1)
   idfm%ymin = simbox(2)
   idfm%xmax = simbox(3)
   idfm%ymax = simbox(4)
   idfm%nodata = nodata(i)
   if (.not.idfreadscale(idfc,idfm,scltype,ismooth)) CALL IMOD_UTL_PRINTTEXT('idfreadscale',2)
   buff = idfm%x
   call idfdeallocatex(idfc)
   if (idfc%iu.gt.0) then
      inquire(unit=idfc%iu,opened=lop); if(lop)close(idfc%iu)
   endif

  ENDIF
  CALL IMOD_UTL_APPLYFCT_R(BUFF,NODATA(I),NROW,NCOL,FCT(I),IMP(I))
  SELECT CASE (I)
   CASE (1);  SIMGRO%IBOUND=INT(BUFF)
   CASE (2);  SIMGRO%LGN=INT(BUFF)
   CASE (3);  SIMGRO%RZ=BUFF
   CASE (4);  SIMGRO%BODEM=INT(BUFF)
   CASE (5);  SIMGRO%METEO=INT(BUFF)
   CASE (6);  SIMGRO%MV=BUFF
   CASE (7);  SIMGRO%BEREGEN=INT(BUFF)
   CASE (8);  SIMGRO%BER_LAAG=INT(BUFF)
   CASE (9);  SIMGRO%BEREGEN_Q=BUFF
   CASE (10); SIMGRO%NOPP=BUFF
   CASE (11); SIMGRO%SOPP=BUFF
   CASE (12); SIMGRO%VXMU_SOPP=BUFF !nap
   CASE (13); SIMGRO%VXMU_ROPP=BUFF !nap
   CASE (14); SIMGRO%CRUNOFF_SOPP=BUFF
   CASE (15); SIMGRO%CRUNOFF_ROPP=BUFF
   CASE (16); SIMGRO%CRUNON_SOPP=BUFF
   CASE (17); SIMGRO%CRUNON_ROPP=BUFF
   CASE (18); SIMGRO%QINFBASIC_SOPP=BUFF
   CASE (19); SIMGRO%QINFBASIC_ROPP=BUFF
   CASE (20); SIMGRO%PWT_LEVEL=BUFF
   CASE (21); SIMGRO%MOISTURE=BUFF
   CASE (22); SIMGRO%COND=BUFF
  END SELECT
 ENDDO

 IF(MMOD(PPWT).NE.1)SIMGRO%PWT_LEVEL=NODATA(20)

 !## make sure corner of model are incative, no connection with MetaSWAP
 SIMGRO(1   ,1   )%IBOUND=0
 SIMGRO(1   ,NROW)%IBOUND=0
 SIMGRO(NCOL,1   )%IBOUND=0
 SIMGRO(NCOL,NROW)%IBOUND=0
 
 !## make sure that for sopp>0 there is a vxmu value, turn nopp otherwise off
 DO IROW=1,NROW; DO ICOL=1,NCOL
  IF(SIMGRO(ICOL,IROW)%VXMU_SOPP.EQ.NODATA(12))SIMGRO(ICOL,IROW)%SOPP=0.0
  IF(SIMGRO(ICOL,IROW)%SOPP.GT.0.0)THEN
   IF(SIMGRO(ICOL,IROW)%VXMU_SOPP     .EQ.NODATA(12))SIMGRO(ICOL,IROW)%SOPP=0.0
   IF(SIMGRO(ICOL,IROW)%CRUNOFF_SOPP  .EQ.NODATA(14))SIMGRO(ICOL,IROW)%SOPP=0.0
   IF(SIMGRO(ICOL,IROW)%CRUNON_SOPP   .EQ.NODATA(16))SIMGRO(ICOL,IROW)%SOPP=0.0
   IF(SIMGRO(ICOL,IROW)%QINFBASIC_SOPP.EQ.NODATA(18))SIMGRO(ICOL,IROW)%SOPP=0.0
  ENDIF
  !##
  DX=DELR(ICOL)-DELR(ICOL-1); DY=DELC(IROW-1)-DELC(IROW); ARND=DX*DY
  IF(SIMGRO(ICOL,IROW)%VXMU_ROPP.EQ.NODATA(13))SIMGRO(ICOL,IROW)%NOPP=DX*DY !## surface water, no metaswap
  ARND=ARND-SIMGRO(ICOL,IROW)%NOPP-SIMGRO(ICOL,IROW)%SOPP
  !## rural area
  IF(ARND.GT.0.0)THEN
   IF(SIMGRO(ICOL,IROW)%VXMU_ROPP     .EQ.NODATA(13))SIMGRO(ICOL,IROW)%NOPP=DX*DY !## surface water, no metaswap
   IF(SIMGRO(ICOL,IROW)%CRUNOFF_ROPP  .EQ.NODATA(15))SIMGRO(ICOL,IROW)%NOPP=DX*DY !## surface water, no metaswap
   IF(SIMGRO(ICOL,IROW)%CRUNON_ROPP   .EQ.NODATA(17))SIMGRO(ICOL,IROW)%NOPP=DX*DY !## surface water, no metaswap
   IF(SIMGRO(ICOL,IROW)%QINFBASIC_ROPP.EQ.NODATA(19))SIMGRO(ICOL,IROW)%NOPP=DX*DY !## surface water, no metaswap
  ENDIF
 ENDDO; ENDDO

 !## check input
 IERROR=0
 DO IROW=1,NROW; DO ICOL=1,NCOL
  IF(SIMGRO(ICOL,IROW)%IBOUND.GT.0)THEN
   IF(SIMGRO(ICOL,IROW)%LGN.EQ.NODATA(2))            IERROR(2) =IERROR(2)+1
   IF(SIMGRO(ICOL,IROW)%RZ.EQ.NODATA(3))             IERROR(3) =IERROR(3)+1
   IF(SIMGRO(ICOL,IROW)%BODEM.EQ.NODATA(4))          IERROR(4) =IERROR(4)+1
   IF(SIMGRO(ICOL,IROW)%METEO.EQ.NODATA(5))          IERROR(5) =IERROR(5)+1
   IF(SIMGRO(ICOL,IROW)%MV.EQ.NODATA(6))             IERROR(6) =IERROR(6)+1
   IF(SIMGRO(ICOL,IROW)%BEREGEN.EQ.NODATA(7))        IERROR(7) =IERROR(7)+1
   IF(IARMWP.EQ.0)THEN
    IF(SIMGRO(ICOL,IROW)%BER_LAAG.EQ.NODATA(8))      IERROR(8) =IERROR(8)+1
    IF(SIMGRO(ICOL,IROW)%BEREGEN_Q.EQ.NODATA(9))     IERROR(9) =IERROR(9)+1
   ENDIF
   IF(SIMGRO(ICOL,IROW)%NOPP.EQ.NODATA(10))          IERROR(10)=IERROR(10)+1
   IF(SIMGRO(ICOL,IROW)%SOPP.EQ.NODATA(11))          IERROR(11)=IERROR(11)+1
   IF(SIMGRO(ICOL,IROW)%VXMU_ROPP.EQ.NODATA(13))     IERROR(13)=IERROR(13)+1
   IF(SIMGRO(ICOL,IROW)%CRUNOFF_SOPP.EQ.NODATA(14))  IERROR(14)=IERROR(14)+1
   IF(SIMGRO(ICOL,IROW)%SOPP.GT.0)THEN
    IF(SIMGRO(ICOL,IROW)%VXMU_SOPP.EQ.NODATA(12))     IERROR(12)=IERROR(12)+1
    IF(SIMGRO(ICOL,IROW)%CRUNON_SOPP.EQ.NODATA(16))   IERROR(16)=IERROR(16)+1
    IF(SIMGRO(ICOL,IROW)%QINFBASIC_SOPP.EQ.NODATA(18))IERROR(18)=IERROR(18)+1
   ENDIF
   IF(SIMGRO(ICOL,IROW)%CRUNOFF_ROPP.EQ.NODATA(15))  IERROR(15)=IERROR(15)+1
   IF(SIMGRO(ICOL,IROW)%CRUNON_ROPP.EQ.NODATA(17))   IERROR(17)=IERROR(17)+1
   IF(SIMGRO(ICOL,IROW)%QINFBASIC_ROPP.EQ.NODATA(19))IERROR(19)=IERROR(19)+1
!   IF(MMOD(PPWT).EQ.1)THEN
!    IF(SIMGRO(ICOL,IROW)%PWT_LEVEL.EQ.NODATA(20))  IERROR(20)=IERROR(20)+1 <--- nodata is niet erg, is er geen PWT aanwezig
!   ENDIF
   IF(SIMGRO(ICOL,IROW)%MOISTURE.EQ.NODATA(21)) then
       IERROR(21)=IERROR(21)+1
   end if
   IF(SIMGRO(ICOL,IROW)%COND.EQ.NODATA(22))          IERROR(22)=IERROR(22)+1
  ENDIF
 ENDDO; ENDDO

 !## construct metaswap input files
 SIMGRO_NROW=NROW; SIMGRO_NCOL=NCOL; SIMGRO_NLAY=NLAY

 !## error in data
 IF(SUM(IERROR).GT.0)THEN
  CALL IMOD_UTL_PRINTTEXT('NodataValues on active modelcells found in :',1)
  CALL IMOD_UTL_PRINTTEXT('- Landuse           '//TRIM(IMOD_UTL_ITOS(IERROR(2))),1)
  CALL IMOD_UTL_PRINTTEXT('- Rootzone          '//TRIM(IMOD_UTL_ITOS(IERROR(3))),1)
  CALL IMOD_UTL_PRINTTEXT('- Soil Types        '//TRIM(IMOD_UTL_ITOS(IERROR(4))),1)
  CALL IMOD_UTL_PRINTTEXT('- Meteo Stations    '//TRIM(IMOD_UTL_ITOS(IERROR(5))),1)
  CALL IMOD_UTL_PRINTTEXT('- Surface Level     '//TRIM(IMOD_UTL_ITOS(IERROR(6))),1)
  CALL IMOD_UTL_PRINTTEXT('- Art. Recharge     '//TRIM(IMOD_UTL_ITOS(IERROR(7))),1)
  CALL IMOD_UTL_PRINTTEXT('- Art. Rch. Layer   '//TRIM(IMOD_UTL_ITOS(IERROR(8))),1)
  CALL IMOD_UTL_PRINTTEXT('- Art. Rch. Strength'//TRIM(IMOD_UTL_ITOS(IERROR(9))),1)
  CALL IMOD_UTL_PRINTTEXT('- Wetted Area       '//TRIM(IMOD_UTL_ITOS(IERROR(10))),1)
  CALL IMOD_UTL_PRINTTEXT('- Surf. Urban Area  '//TRIM(IMOD_UTL_ITOS(IERROR(11))),1)
  CALL IMOD_UTL_PRINTTEXT('- VXMU SOPP         '//TRIM(IMOD_UTL_ITOS(IERROR(12))),1)
  CALL IMOD_UTL_PRINTTEXT('- VXMU ROPP         '//TRIM(IMOD_UTL_ITOS(IERROR(13))),1)
  CALL IMOD_UTL_PRINTTEXT('- CRUNOFF SOPP      '//TRIM(IMOD_UTL_ITOS(IERROR(14))),1)
  CALL IMOD_UTL_PRINTTEXT('- CRUNOFF ROPP      '//TRIM(IMOD_UTL_ITOS(IERROR(15))),1)
  CALL IMOD_UTL_PRINTTEXT('- CRUNON SOPP       '//TRIM(IMOD_UTL_ITOS(IERROR(16))),1)
  CALL IMOD_UTL_PRINTTEXT('- CRUNON ROPP       '//TRIM(IMOD_UTL_ITOS(IERROR(17))),1)
  CALL IMOD_UTL_PRINTTEXT('- QINFBASIS SOPP    '//TRIM(IMOD_UTL_ITOS(IERROR(18))),1)
  CALL IMOD_UTL_PRINTTEXT('- QINFBASIS ROPP    '//TRIM(IMOD_UTL_ITOS(IERROR(19))),1)
!  CALL IMOD_UTL_PRINTTEXT('- Pondingdepth      '//TRIM(IMOD_UTL_ITOS(IERROR(12))),1)
!  IF(MMOD(PPWT).EQ.1)CALL IMOD_UTL_PRINTTEXT('- PWT Level         '//TRIM(IMOD_UTL_ITOS(IERROR(20))),1)
  CALL IMOD_UTL_PRINTTEXT('- Moisture Factor   '//TRIM(IMOD_UTL_ITOS(IERROR(21))),1)
  CALL IMOD_UTL_PRINTTEXT('- Conductivity      '//TRIM(IMOD_UTL_ITOS(IERROR(22))),1)
  CALL IMOD_UTL_PRINTTEXT('Process stopped!',2)
 ENDIF

! call gwf2met1ibound(simgro%ibound,ncol,nrow,nlay,1)               ! MET
 CALL METASWAP_IBND(SIMGRO%IBOUND,NROW,NCOL,IFULL)

 !## construct metaswap files
 CALL METASWAP_EXPORT4(NODATA(20),FNAME(8))

 IF (.FALSE.) THEN
  DX=DELR(1)-DELR(0); DY=DELC(0)-DELC(1)
  OK=IDFWRITE_WRAPPER(NCOL,NROW,REAL(SIMGRO%IBOUND),        (/DX/),(/DY/),SIMBOX(1),SIMBOX(2),NODATA(1),'','msbnd.idf')
  OK=IDFWRITE_WRAPPER(NCOL,NROW,REAL(SIMGRO%LGN),           (/DX/),(/DY/),SIMBOX(1),SIMBOX(2),NODATA(1),'','mslgn.idf')
  OK=IDFWRITE_WRAPPER(NCOL,NROW,REAL(SIMGRO%RZ),            (/DX/),(/DY/),SIMBOX(1),SIMBOX(2),NODATA(1),'','msrtz.idf')
  OK=IDFWRITE_WRAPPER(NCOL,NROW,REAL(SIMGRO%BODEM),         (/DX/),(/DY/),SIMBOX(1),SIMBOX(2),NODATA(1),'','mssfu.idf')
  OK=IDFWRITE_WRAPPER(NCOL,NROW,REAL(SIMGRO%METEO),         (/DX/),(/DY/),SIMBOX(1),SIMBOX(2),NODATA(1),'','msmet.idf')
  OK=IDFWRITE_WRAPPER(NCOL,NROW,REAL(SIMGRO%MV),            (/DX/),(/DY/),SIMBOX(1),SIMBOX(2),NODATA(1),'','mssev.idf')
  OK=IDFWRITE_WRAPPER(NCOL,NROW,REAL(SIMGRO%BEREGEN),       (/DX/),(/DY/),SIMBOX(1),SIMBOX(2),NODATA(1),'','msart.idf')
  OK=IDFWRITE_WRAPPER(NCOL,NROW,REAL(SIMGRO%BER_LAAG),      (/DX/),(/DY/),SIMBOX(1),SIMBOX(2),NODATA(1),'','msarl.idf')
  OK=IDFWRITE_WRAPPER(NCOL,NROW,REAL(SIMGRO%BEREGEN_Q),     (/DX/),(/DY/),SIMBOX(1),SIMBOX(2),NODATA(1),'','msarc.idf')
  OK=IDFWRITE_WRAPPER(NCOL,NROW,REAL(SIMGRO%NOPP),          (/DX/),(/DY/),SIMBOX(1),SIMBOX(2),NODATA(1),'','mswta.idf')
  OK=IDFWRITE_WRAPPER(NCOL,NROW,REAL(SIMGRO%SOPP),          (/DX/),(/DY/),SIMBOX(1),SIMBOX(2),NODATA(1),'','msuba.idf')
  OK=IDFWRITE_WRAPPER(NCOL,NROW,REAL(SIMGRO%VXMU_SOPP),     (/DX/),(/DY/),SIMBOX(1),SIMBOX(2),NODATA(1),'','mspdu.idf')
  OK=IDFWRITE_WRAPPER(NCOL,NROW,REAL(SIMGRO%VXMU_ROPP),     (/DX/),(/DY/),SIMBOX(1),SIMBOX(2),NODATA(1),'','mspdr.idf')
  OK=IDFWRITE_WRAPPER(NCOL,NROW,REAL(SIMGRO%CRUNOFF_SOPP),  (/DX/),(/DY/),SIMBOX(1),SIMBOX(2),NODATA(1),'','msofu.idf')
  OK=IDFWRITE_WRAPPER(NCOL,NROW,REAL(SIMGRO%CRUNOFF_ROPP),  (/DX/),(/DY/),SIMBOX(1),SIMBOX(2),NODATA(1),'','msofr.idf')
  OK=IDFWRITE_WRAPPER(NCOL,NROW,REAL(SIMGRO%CRUNON_SOPP),   (/DX/),(/DY/),SIMBOX(1),SIMBOX(2),NODATA(1),'','msonu.idf')
  OK=IDFWRITE_WRAPPER(NCOL,NROW,REAL(SIMGRO%CRUNON_ROPP),   (/DX/),(/DY/),SIMBOX(1),SIMBOX(2),NODATA(1),'','msonr.idf')
  OK=IDFWRITE_WRAPPER(NCOL,NROW,REAL(SIMGRO%QINFBASIC_SOPP),(/DX/),(/DY/),SIMBOX(1),SIMBOX(2),NODATA(1),'','msqiu.idf')
  OK=IDFWRITE_WRAPPER(NCOL,NROW,REAL(SIMGRO%QINFBASIC_ROPP),(/DX/),(/DY/),SIMBOX(1),SIMBOX(2),NODATA(1),'','msqir.idf')
  OK=IDFWRITE_WRAPPER(NCOL,NROW,REAL(SIMGRO%PWT_LEVEL),     (/DX/),(/DY/),SIMBOX(1),SIMBOX(2),NODATA(1),'','mspwt.idf')
  OK=IDFWRITE_WRAPPER(NCOL,NROW,REAL(SIMGRO%MOISTURE),      (/DX/),(/DY/),SIMBOX(1),SIMBOX(2),NODATA(1),'','mssfc.idf')
  OK=IDFWRITE_WRAPPER(NCOL,NROW,REAL(SIMGRO%COND),          (/DX/),(/DY/),SIMBOX(1),SIMBOX(2),NODATA(1),'','mscfc.idf')
 END IF

 IF(IAREA.GT.0)CLOSE(IAREA)
 IF(ISELSVAT.GT.0)CLOSE(ISELSVAT)
 IF(INDSB.GT.0)CLOSE(INDSB)
 IF(ISCAP.GT.0)CLOSE(ISCAP)
 IF(IGWMP.GT.0)CLOSE(IGWMP)
 IF(IMODSIM.GT.0)CLOSE(IMODSIM)
 IF(IINFI.GT.0)CLOSE(IINFI)
 IF(IIDF.GT.0) CLOSE(IIDF)
 IF(IUSCL.GT.0)CLOSE(IUSCL)

 CALL IMOD_UTL_PRINTTEXT('Copying input files metaswap ...',0)
 DO ICOL=NIDF+1-(IARMWP*1),NLINES

  READ(IURUN,'(A256)') FNAME(1)
  CALL IMOD_UTL_STRING(FNAME(1))
  CALL IMOD_UTL_FILENAME(FNAME(1))
  CALL IMOD_UTL_MODEL1CHECKFNAME(FNAME(1),0)
  FNAME(2)=FNAME(1)
  CALL IMOD_UTL_CAP(FNAME(2),'U')
  IF(INDEX(FNAME(2),'PARA_SIM.INP').GT.0)THEN
   CALL METASWAP_PARASIM(FNAME(1),trim(simwd)//'para_sim.inp')
  ELSE
   CALL IMOD_UTL_PRINTTEXT('Copying file "'//TRIM(FNAME(1))//'" ...',0)
   cl = ''
   if (os.eq.1) then
     cl = 'COPY "'//TRIM(FNAME(1))//'" "'//trim(simwd)//'." /Y '
   else if (os.eq.2) then
     cl = 'cp "'//TRIM(FNAME(1))//'" "'//trim(simwd)//'"'
   end if
   if(len_trim(cl).eq.len(cl))then
    write(*,'(a)') 'Cannot copy file - increase length of string cl'; stop
   endif
   CALL SYSTEM(trim(cl))
  ENDIF
 END DO

 CALL METASWAP_METEGRID(trim(simwd))

 DEALLOCATE(FNAME,SIMGRO,BUFF,ITMP,FCT,IMP,CONSTANTE,NODATA,IERROR)

 CALL IMOD_UTL_PRINTTEXT('Finished processing MetaSwap ...',0)

 END SUBROUTINE RF2MF_METASWAP

!###====================================================================
SUBROUTINE METASWAP_IBND(IBOUND,NROW,NCOL,IFULL)
!###====================================================================
!USE MODFLOW
IMPLICIT NONE
INTEGER,INTENT(IN) :: NROW,NCOL
INTEGER,INTENT(IN),DIMENSION(4) :: IFULL
INTEGER,DIMENSION(NCOL,NROW),INTENT(INOUT) :: IBOUND
INTEGER :: IROW,ICOL

DO IROW=1,NROW
 IF(IFULL(1).EQ.0)THEN
  ICOL=1
  IF(IBOUND(ICOL,IROW).GT.0)IBOUND(ICOL,IROW)=-1
 ENDIF
 IF(IFULL(3).EQ.0)THEN
  ICOL=NCOL
  IF(IBOUND(ICOL,IROW).GT.0)IBOUND(ICOL,IROW)=-1
 ENDIF
ENDDO
DO ICOL=1,NCOL
 IF(IFULL(4).EQ.0)THEN
  IROW=1
  IF(IBOUND(ICOL,IROW).GT.0)IBOUND(ICOL,IROW)=-1
 ENDIF
 IF(IFULL(2).EQ.0)THEN
  IROW=NROW
  IF(IBOUND(ICOL,IROW).GT.0)IBOUND(ICOL,IROW)=-1
 ENDIF
ENDDO

RETURN
END SUBROUTINE

 !###====================================================================
 SUBROUTINE METASWAP_EXPORT4(NODATA_PWT,IPFFILE)
 !###====================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: NODATA_PWT
 CHARACTER(LEN=*),INTENT(IN) :: IPFFILE
 INTEGER,PARAMETER :: AEND=0 !## afwateringseenheid --- hebben we niet
 INTEGER :: NUND,MDND,MDND2,IROW,ICOL,LYBE,TYBE,IC1,IC2,IR1,IR2,BEREGENID,JROW,JCOL,N,M,I,JU
 REAL :: ARND,XC,YC,DX,DY,FLBE,QBER
 TYPE IPFOBJ
  INTEGER :: ILAY
  REAL :: X,Y,CAP
 END TYPE IPFOBJ
 TYPE(IPFOBJ),ALLOCATABLE,DIMENSION(:) :: IPF
 logical :: lurban
 integer :: mndxc, ndxc, unid
 integer, dimension(:,:), allocatable :: dxcid

 if (allocated(dxcid)) deallocate(dxcid)
 mndxc = nrow*ncol*nlay
 allocate(dxcid(mndxc,4))
 ndxc = 0

 !## check input parameters
 CALL METASWAP_CHECK()

 IF(IARMWP.EQ.1)THEN
  JU=GETUNIT(); MDND=0
  DO IR1=1,2
   CALL IMOD_UTL_OPENASC(JU,IPFFILE,'R')
   READ(JU,*) N; READ(JU,*) M
   IF(M.LT.5)CALL IMOD_UTL_PRINTTEXT('IPF for artificial recharge should be at least 5 column, x,y,ilay,id,capacity',2)
   DO I=1,M+1; READ(JU,*) ; ENDDO
   IF(IR1.EQ.2)ALLOCATE(IPF(MDND))
   DO I=1,N
    READ(JU,*) XC,YC,LYBE,NUND,QBER
    IF(IR1.EQ.1)MDND=MAX(MDND,NUND)
    IF(IR1.EQ.2)THEN; IPF(NUND)%X=XC; IPF(NUND)%Y=YC; IPF(NUND)%ILAY=LYBE; IPF(NUND)%CAP=QBER; ENDIF
   ENDDO
   CLOSE(JU)
  ENDDO
  CALL IMOD_UTL_PRINTTEXT('Reading '//TRIM(IMOD_UTL_ITOS(N))//' records from '//TRIM(IPFFILE),0)
 ENDIF

 !## get window of interest
 CALL IMOD_UTL_POL1LOCATER(DELR,NCOL+1,USEBOX(1)+IDFTINY,IC1)
 CALL IMOD_UTL_POL1LOCATER(DELR,NCOL+1,USEBOX(3)-IDFTINY,IC2)
 CALL IMOD_UTL_POL1LOCATER(DELC,NROW+1,USEBOX(4)-IDFTINY,IR1)
 CALL IMOD_UTL_POL1LOCATER(DELC,NROW+1,USEBOX(2)+IDFTINY,IR2)
 !## check to make sure dimensions are within bounds!
 IC1=MAX(1,IC1); IC2=MIN(IC2,NCOL)
 IR1=MAX(1,IR1); IR2=MIN(IR2,NROW)

 NUND=0; unid = 0
 DO IROW=1,SIMGRO_NROW
  DO ICOL=1,SIMGRO_NCOL
   
   lurban=.false.
   IF(SIMGRO(ICOL,IROW)%IBOUND.LE.0)CYCLE
   MDND=(IROW-1)*SIMGRO_NCOL+ICOL

   DX=DELR(ICOL)-DELR(ICOL-1)
   DY=DELC(IROW-1)-DELC(IROW)

   XC  =(DELR(ICOL)+DELR(ICOL-1))/2.0
   YC  =(DELC(IROW)+DELC(IROW-1))/2.0
   ARND= DX*DY
   ARND= ARND-SIMGRO(ICOL,IROW)%NOPP-SIMGRO(ICOL,IROW)%SOPP

   !## rural area > 0
   IF(ARND.GT.0.0)THEN

    lurban=.true.
    NUND=NUND+1

    !## write idf_svat.inp - inside area of interest
    IF(ICOL.GE.IC1.AND.ICOL.LE.IC2.AND. &
       IROW.GE.IR1.AND.IROW.LE.IR2)WRITE(IIDF,'(3I10)') NUND,IROW-IR1+1,ICOL-IC1+1

    !## write sel_svat_bda.inp
    WRITE(ISELSVAT,'(I10)') NUND

    !## write area_svat.inp
    WRITE(IAREA,'(I10,F10.1,F8.3,8X,I6,8X,8X,I6,F8.3,I10,2F8.3)') NUND,ARND,SIMGRO(ICOL,IROW)%MV, &
          SIMGRO(ICOL,IROW)%BODEM,SIMGRO(ICOL,IROW)%LGN,SIMGRO(ICOL,IROW)%RZ/100.0,               &
          SIMGRO(ICOL,IROW)%METEO,1.0,1.0

    !## write svat2swnr_roff.inp ------------------
    WRITE(INDSB,'(I10,I10,F8.3,2F8.1)') NUND,AEND,SIMGRO(ICOL,IROW)%VXMU_ROPP,SIMGRO(ICOL,IROW)%CRUNOFF_ROPP, &
                                        SIMGRO(ICOL,IROW)%CRUNON_ROPP !, 1.0,1.0

    !## write infi_svat.inp, infiltratiecapaciteit per cel, de rest -9999.
    WRITE(IINFI,'(I10,F8.3,4F8.1)') NUND,SIMGRO(ICOL,IROW)%QINFBASIC_ROPP,-9999.0,-9999.0,-9999.0,-9999.0

    !## add couple location modflow
    unid=unid+1; call storedxc(dxcid,mndxc,ndxc,1,irow,icol,unid)
    !## write coupling table
    WRITE(IGWMP  ,'(I10,2X,I10,I5)')  unid,NUND,1
    WRITE(IMODSIM,'(I10,2X,I10,I5)')  unid,NUND,1

    !## BEGIN scap_svat.inp - grondwater + ow

    IF(IARMWP.EQ.0)THEN
     LYBE=SIMGRO(ICOL,IROW)%BER_LAAG
     TYBE=SIMGRO(ICOL,IROW)%BEREGEN
     QBER=SIMGRO(ICOL,IROW)%BEREGEN_Q
     JCOL=ICOL; JROW=IROW
    ELSE
     JCOL=0; JROW=0
     BEREGENID=INT(SIMGRO(ICOL,IROW)%BEREGEN)
     IF(BEREGENID.GT.0.AND.BEREGENID.LE.SIZE(IPF))THEN
      QBER=IPF(BEREGENID)%CAP
      LYBE=IPF(BEREGENID)%ILAY
      TYBE=1 !## groundwater
      CALL IMOD_UTL_POL1LOCATER(DELR,NCOL+1,IPF(BEREGENID)%X,JCOL)
      CALL IMOD_UTL_POL1LOCATER(DELC,NROW+1,IPF(BEREGENID)%Y,JROW)
      IF(JCOL.LT.0.OR.JCOL.GT.NCOL)JCOL=0
      IF(JROW.LT.0.OR.JROW.GT.NROW)JROW=0
     ENDIF
    ENDIF
    MDND2=      (JROW-1)*SIMGRO_NCOL+JCOL
    MDND2=MDND2+(LYBE-1)*SIMGRO_NCOL*SIMGRO_NROW

    IF(JROW.NE.0.AND.JCOL.NE.0)THEN

     FLBE=0.0
     IF(TYBE.EQ.1)THEN
      FLBE=QBER  !## maximum groundwater   abstraction mm/day fmmxabgw
     ELSEIF(TYBE.EQ.2)THEN
      FLBE=QBER  !## maximum surface water abstraction mm/day fmmxabsw
     ENDIF

     IF(FLBE.GT.0.0)THEN
      IF(TYBE.EQ.1)THEN
       WRITE(ISCAP,'(I10,F8.2,24X,I10,I6)') NUND,FLBE,NUND,LYBE
      ELSEIF(TYBE.EQ.2)THEN
       WRITE(ISCAP,'(I10,8X,F8.2,32X,I10)') NUND,FLBE,AEND
      ENDIF
     ENDIF

     !## sprinkling from other than modellayer 1 or other location
     IF(TYBE.EQ.1.AND.LYBE.GT.1)THEN

      !## add couple location modflow
      unid=unid+1; call storedxc(dxcid,mndxc,ndxc,lybe,jrow,jcol,unid)
      !## write coupling table
      WRITE(IGWMP  ,'(I10,2X,I10,I5)') unid,NUND,LYBE
      WRITE(IMODSIM,'(I10,2X,I10,I5)') unid,NUND,LYBE

     ENDIF

    ENDIF

    IF(MMOD(PPWT).EQ.0)WRITE(IUSCL,'(I10,3F8.3,8X,2I10)')   NUND,SIMGRO(ICOL,IROW)%MOISTURE,SIMGRO(ICOL,IROW)%COND,1.0,ICOL,IROW
    IF(MMOD(PPWT).EQ.1)THEN
     IF(SIMGRO(ICOL,IROW)%PWT_LEVEL.NE.NODATA_PWT)THEN
      WRITE(IUSCL,'(I10,4F8.3,2I10)')    NUND,SIMGRO(ICOL,IROW)%MOISTURE,SIMGRO(ICOL,IROW)%COND,1.0, &
                                         SIMGRO(ICOL,IROW)%MV-SIMGRO(ICOL,IROW)%PWT_LEVEL,ICOL,IROW
     ELSE
      WRITE(IUSCL,'(I10,3F8.3,8X,2I10)') NUND,SIMGRO(ICOL,IROW)%MOISTURE,SIMGRO(ICOL,IROW)%COND,1.0,ICOL,IROW
     ENDIF
    ENDIF

   ENDIF

   !## urban area (verhard)
   ARND  =DX*DY
   ARND  =MIN(ARND,SIMGRO(ICOL,IROW)%SOPP) !< dit komt niet meer terug?
   IF(ARND.GT.0.0)THEN
    NUND=NUND+1

    !## write idf_svat.inp - inside area of interest
    IF(ICOL.GE.IC1.AND.ICOL.LE.IC2.AND. &
       IROW.GE.IR1.AND.IROW.LE.IR2)WRITE(IIDF,'(3I10)') NUND,IROW-IR1+1,ICOL-IC1+1
       
    !## write sel_svat_bda.inp
    WRITE(ISELSVAT,'(I10)') NUND

    WRITE(IAREA,'(I10,F10.1,F8.3,8X,I6,16X,I6,F8.3,I10,2F8.2)') &  !
      NUND,ARND,SIMGRO(ICOL,IROW)%MV+MSWPMV,SIMGRO(ICOL,IROW)%BODEM,18,0.1,SIMGRO(ICOL,IROW)%METEO,1.0,1.0

    WRITE(INDSB,'(2I10,F8.3,2F8.1)') NUND,0,SIMGRO(ICOL,IROW)%VXMU_SOPP,SIMGRO(ICOL,IROW)%CRUNOFF_SOPP,SIMGRO(ICOL,IROW)%CRUNON_SOPP !1.0,1.0

    !## add couple location modflow - only if not yet urban location added
    if(.not.lurban)then
     unid=unid+1; call storedxc(dxcid,mndxc,ndxc,1,irow,icol,unid)
    endif

    !## write coupling table
    WRITE(IGWMP  ,'(I10,2X,I10,I5)') unid,NUND,1
    WRITE(IMODSIM,'(I10,2X,I10,I5)') unid,NUND,1

    IF(MMOD(PPWT).EQ.0)WRITE(IUSCL,'(I10,3F8.3,8X,2I10)') NUND,SIMGRO(ICOL,IROW)%MOISTURE,SIMGRO(ICOL,IROW)%COND,1.0,ICOL,IROW
    IF(MMOD(PPWT).EQ.1)THEN
     IF(SIMGRO(ICOL,IROW)%PWT_LEVEL.NE.NODATA_PWT)THEN
      WRITE(IUSCL,'(I10,4F8.3,2I10)')   NUND,SIMGRO(ICOL,IROW)%MOISTURE,SIMGRO(ICOL,IROW)%COND,1.0, &
                                        SIMGRO(ICOL,IROW)%MV-SIMGRO(ICOL,IROW)%PWT_LEVEL,ICOL,IROW
     ELSE
      WRITE(IUSCL,'(I10,3F8.3,8X,2I10)')   NUND,SIMGRO(ICOL,IROW)%MOISTURE,SIMGRO(ICOL,IROW)%COND,1.0,ICOL,IROW
     ENDIF
    ENDIF

    !## write infi_svat.inp, infiltratiecapaciteit per cel, de rest -9999.
    WRITE(IINFI,'(I10,F8.3,4F8.1)') NUND,SIMGRO(ICOL,IROW)%QINFBASIC_SOPP,-9999.0,-9999.0,-9999.0,-9999.0

   ENDIF

  ENDDO
 ENDDO
 
 call writedxc(idxc,dxcid,mndxc,ndxc)
 deallocate(dxcid)

 IF(IARMWP.EQ.1)DEALLOCATE(IPF)

 RETURN

 END SUBROUTINE METASWAP_EXPORT4

 !###====================================================================
 SUBROUTINE METASWAP_PARASIM(FNAME,FNAME2)
 !###====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 CHARACTER(LEN=*),INTENT(IN) :: FNAME2
 INTEGER :: IU,JU,I,IOS,IC1,IC2,IR1,IR2,SNROW,SNCOL

 I=INDEX(FNAME,'\',.TRUE.)

 IU=GETUNIT(); OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='READ')
 JU=GETUNIT(); OPEN(JU,FILE=FNAME2,STATUS='REPLACE',ACTION='WRITE')
 DO
  READ(IU,'(A256)',IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT
  WRITE(JU,'(A)') TRIM(LINE)
 ENDDO

! CLOSE(IU); CLOSE(JU)
! RETURN

! WRITE(IU,'(A)') '*'
! WRITE(IU,'(A)') '* Parameters for modelling options'
! WRITE(IU,'(A)') '*'
! WRITE(IU,'(A)') '      vegetation_mdl         =      1    ! Vegetation model     (1/2/3) 1= simple, 2= WOFOST,     3=2 + feedback'
! WRITE(IU,'(A)') '      evapotranspiration_mdl =      1    ! Evapotranspiration model (1/2/3) 1=simple, 2=PenMon as ETref,3=PenMonF'
! WRITE(IU,'(A)') '      saltstress_mdl         =      0    ! Salt stress model     (0/1) 0= none    1 = Maas-Hoffman'
! WRITE(IU,'(A)') '      surfacewater_mdl       =      0    ! Surface water model (0/1/2/5) 0= none 1= Surfw 2= + Sobek 5= +SWQN'
! WRITE(IU,'(A)') '*'
! WRITE(IU,'(A)') '* Location of soil physical database (including thsat_svat, beta2_svat.inp)'
! WRITE(IU,'(A)') '      unsa_svat_path         =    "'//TRIM(FNAME(:I))//'" ! " " uses files in work directory;include "\" '
! WRITE(IU,'(A)') '*'
! WRITE(IU,'(A)') '* Parameters for time steps'
! WRITE(IU,'(A)') '*'
! WRITE(IU,'(A)') '      dtgw                   =   1.00    ! Groundwater/soil water time step'
! WRITE(IU,'(A)') '      dtsw                   =   1.00    ! Time step fast processes'
! WRITE(IU,'(A)') '*'
! WRITE(IU,'(A)') '* Parameters for smoothing storage coefficients'
! WRITE(IU,'(A)') '*'
! WRITE(IU,'(A)') '      iterur1                =      3    ! Outer cycle iteration for start of smoothing'
! WRITE(IU,'(A)') '      iterur2                =      5    ! Outer cycle iteration with full smoothing'
! WRITE(IU,'(A)') '*'
! WRITE(IU,'(A)') '* Starting time in meteo-file'
! WRITE(IU,'(A)') '*'
! WRITE(IU,'(A)') '      idbg                   =   0.00    ! Starting time in meteo file'
! WRITE(IU,'(A)') '      iybg                   =   1994'
! WRITE(IU,'(A)') '*'
! WRITE(IU,'(A)') '* Parameters for indicating summer period water management'
! WRITE(IU,'(A)') '*'
! WRITE(IU,'(A)') '      tdbgsm                 =   0.00    ! Beginning of summer water management period'
! WRITE(IU,'(A)') '      tdedsm                 = 366.00    ! End of summer water management period'

! WRITE(JU,'(A)') '*'
! WRITE(JU,'(A)') '*  Parameters for output'
! WRITE(JU,'(A)') '*'
! WRITE(JU,'(A)') '      clocktime              =      0    ! Produces files for clocking cpu/realtime used'
! WRITE(JU,'(A)') '      svat_gt                =      0    ! File  with 14-day gw. levels'
! WRITE(JU,'(A)') '      svat_per               =      0    ! File  with period-info SVATs'
! WRITE(JU,'(A)') '      svat_per_csv           =      0    ! Files with period-info selected SVATs'
! WRITE(JU,'(A)') '      svat_dtgw              =      0    ! File  with dtgw-info SVATs'
! WRITE(JU,'(A)') '      svat_dtgw_csv          =      0    ! Files with dtgw-info selected SVATs'
! WRITE(JU,'(A)') '      svat2gw_dtgw           =      0    ! File  with dtgw-info flow to GW as system volume'
! WRITE(JU,'(A)') '      svat_vg_per            =      0    ! File  with period-info vegetation model'
! WRITE(JU,'(A)') '      svat_vg_per_csv        =      0    ! File  with period-info vegetation model selected SVATs'
! WRITE(JU,'(A)') '      svat_vg_day            =      0    ! File  with day-info vegetation model'
! WRITE(JU,'(A)') '      svat_vg_day_csv        =      0    ! File  with day-info vegetation model selected SVATs'
! WRITE(JU,'(A)') '      drng_per               =      0    ! File  with period-info drainage links'
! WRITE(JU,'(A)') '      sw_per                 =      0    ! File  with period-info Surfw'
! WRITE(JU,'(A)') '      sw_per_csv             =      0    ! Files with period-info selected Surfw nr"s'
! WRITE(JU,'(A)') '      sw_dtgw                =      0    ! File  with dtgw-info Surfw'
! WRITE(JU,'(A)') '      sw_dtgw_csv            =      0    ! Files with dtgw-info selected Surfw nr"s'
! WRITE(JU,'(A)') '      sw_hq_dtgw             =      0    ! File  with dtgw-info H,Q  of Surfw'
! WRITE(JU,'(A)') '      sw_dtsw                =      0    ! File  with dtsw-info Surfw'
! WRITE(JU,'(A)') '      sw_hq_dtsw             =      0    ! File  with dtsw-info H,Q  of Surfw'
! WRITE(JU,'(A)') '      svat_per_unf           =      0    ! File  with period-info SVAT for postmetaswap'
! WRITE(JU,'(A)') '      modf_per_unf           =      0    ! File  with period-info MODFLOW for postmetaswap'
! WRITE(JU,'(A)') '      sw_dtgw_unf            =      0    ! File  with dtgw-info Surfw for water quality'

 CALL IMOD_UTL_POL1LOCATER(DELR,NCOL+1,USEBOX(1)+IDFTINY,IC1)
 CALL IMOD_UTL_POL1LOCATER(DELR,NCOL+1,USEBOX(3)-IDFTINY,IC2)
 CALL IMOD_UTL_POL1LOCATER(DELC,NROW+1,USEBOX(4)-IDFTINY,IR1)
 CALL IMOD_UTL_POL1LOCATER(DELC,NROW+1,USEBOX(2)+IDFTINY,IR2)

 !## check to make sure dimensions are within bounds!
 IC1  = MAX(1,IC1); IC2  = MIN(IC2,NCOL)
 IR1  = MAX(1,IR1); IR2  = MIN(IR2,NROW)
 SNCOL=(IC2-IC1)+1; SNROW=(IR2-IR1)+1

 WRITE(JU,'(A)') '*'
 WRITE(JU,'(A)') '*  Parameters for IDF output'
 WRITE(JU,'(A)') '*'
 WRITE(JU,'(A)') '      idf_per                =      1    ! Writing IDF files'
 LINE='      idf_xmin                =      '//TRIM(IMOD_UTL_RTOS(USEBOX(1),'F',2))
 WRITE(JU,'(A)') TRIM(LINE)
 LINE='      idf_ymin                =      '//TRIM(IMOD_UTL_RTOS(USEBOX(2),'F',2))
 WRITE(JU,'(A)') TRIM(LINE)
 LINE='      idf_dx                  =      '//TRIM(IMOD_UTL_RTOS(SIMCSIZE,'F',2))
 WRITE(JU,'(A)') TRIM(LINE)
 LINE='      idf_dy                  =      '//TRIM(IMOD_UTL_RTOS(SIMCSIZE,'F',2))
 WRITE(JU,'(A)') TRIM(LINE)
 LINE='      idf_ncol                =      '//TRIM(IMOD_UTL_ITOS(SNCOL))
 WRITE(JU,'(A)') TRIM(LINE)
 LINE='      idf_nrow                =      '//TRIM(IMOD_UTL_ITOS(SNROW))
 WRITE(JU,'(A)') TRIM(LINE)
 LINE='      idf_nodata              =      '//TRIM(IMOD_UTL_RTOS(-9999.99,'F',2))
 WRITE(JU,'(A)') TRIM(LINE)

 CLOSE(JU)

 END SUBROUTINE METASWAP_PARASIM

 !###====================================================================
 SUBROUTINE METASWAP_METEGRID(simwd)
 !###====================================================================
 IMPLICIT NONE

 character(len=*) :: simwd

 LOGICAL :: LEX
 INTEGER :: IGRID,IY
 REAL :: TD
 CHARACTER(LEN=256) :: PRECFNAME,ETFNAME

 !## inquire the existence of mete_grid.inp
 INQUIRE(FILE=trim(simwd)//'mete_grid.inp',EXIST=LEX)
 IF(.NOT.LEX)RETURN

 !## open mete_grid.inp
 CALL IMOD_UTL_OPENASC(IGRID,trim(simwd)//'mete_grid.inp','R')
 READ(IGRID,*) TD,IY,PRECFNAME,ETFNAME

 CALL METASWAP_METEGRID_INP(PRECFNAME,trim(simwd)//'svat2PrecGrid.inp')
 CALL METASWAP_METEGRID_INP(ETFNAME,  trim(simwd)//'svat2EtrefGrid.inp')

 END SUBROUTINE

 !###====================================================================
 SUBROUTINE METASWAP_METEGRID_INP(ASCIIFNAME,INPFNAME)
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: IU,A_NROW,A_NCOL,IROW,ICOL,I,IR1,IR2,IC1,IC2,NUND
 CHARACTER(LEN=*),INTENT(IN) :: ASCIIFNAME,INPFNAME
 REAL :: A_XLLC,A_YLLC,A_NODATA,A_CELLSIZE,IX,IY,DX,DY,ARND
 CHARACTER(LEN=52) :: TXT
 INTEGER,ALLOCATABLE,DIMENSION(:,:) :: PDELR,PDELC

 IF(ALLOCATED(PDELR))DEALLOCATE(PDELR)
 IF(ALLOCATED(PDELC))DEALLOCATE(PDELC)
 ALLOCATE(PDELR(2,NCOL),PDELC(2,NROW))

 !## read header of ascii file
 CALL IMOD_UTL_OPENASC(IU,ASCIIFNAME,'R')
 READ(IU,*) TXT,A_NCOL
 READ(IU,*) TXT,A_NROW
 READ(IU,*) TXT,A_XLLC
 TXT=IMOD_UTL_CAPF(TXT,'U');IX=0.0; IF(TRIM(TXT).EQ.'XLLCENTER')IX=1.0
 READ(IU,*) TXT,A_YLLC
 TXT=IMOD_UTL_CAPF(TXT,'U'); IY=0.0; IF(TRIM(TXT).EQ.'YLLCENTER')IY=1.0
 READ(IU,*) TXT,A_CELLSIZE
 READ(IU,*) TXT,A_NODATA
 A_XLLC=A_XLLC-(IX*(A_CELLSIZE/2.0)); A_YLLC=A_YLLC-(IY*(A_CELLSIZE/2.0))

 CLOSE(IU)

 CALL IMOD_UTL_SCALE1PDELRC(A_XLLC,A_YLLC,A_XLLC+(A_NCOL*A_CELLSIZE),A_YLLC+(A_NROW*A_CELLSIZE), &
           DELC,DELR,PDELR,PDELC,NROW,NCOL,A_CELLSIZE,A_NROW,A_NCOL,0,0,0)

 !## write koppeltabel
 CALL IMOD_UTL_OPENASC(IU,INPFNAME,'W')

 !## fill svat connection to recharge/et based upon svat-units
 NUND=0
 DO IROW=1,SIMGRO_NROW
  DY=DELC(IROW-1)-DELC(IROW)
  DO ICOL=1,SIMGRO_NCOL
   DX    = DELR(ICOL)-DELR(ICOL-1)
   ARND  = DX*DY
   ARND  = ARND-SIMGRO(ICOL,IROW)%NOPP-SIMGRO(ICOL,IROW)%SOPP
   IF(SIMGRO(ICOL,IROW)%IBOUND.GT.0.AND.ARND.GT.0.0)THEN
    NUND  =NUND+1
    IR1=PDELC(1,IROW); IF(IR1.LT.0)IR1=PDELC(1,ABS(IR1))
    IR2=PDELC(2,IROW); IF(IR2.LT.0)IR2=PDELC(2,ABS(IR2))
    IC1=PDELR(1,ICOL); IF(IC1.LT.0)IC1=PDELR(1,ABS(IC1))
    IC2=PDELR(2,ICOL); IF(IC2.LT.0)IC2=PDELR(2,ABS(IC2))
    WRITE(IU,'(3I10,10X,2I10)') NUND,IR1,IC1,IR2,IC2
   ENDIF
   !## urban area
   ARND  = DX*DY
   ARND  = MIN(ARND,SIMGRO(ICOL,IROW)%SOPP)
   IF(SIMGRO(ICOL,IROW)%IBOUND.GT.0.AND.ARND.GT.0.0)THEN
    NUND=NUND+1
    IR1=PDELC(1,IROW); IF(IR1.LT.0)IR1=PDELC(1,ABS(IR1))
    IR2=PDELC(2,IROW); IF(IR2.LT.0)IR2=PDELC(2,ABS(IR2))
    IC1=PDELR(1,ICOL); IF(IC1.LT.0)IC1=PDELR(1,ABS(IC1))
    IC2=PDELR(2,ICOL); IF(IC2.LT.0)IC2=PDELR(2,ABS(IC2))
    WRITE(IU,'(3I10,10X,2I10)') NUND,IR1,IC1,IR2,IC2
   ENDIF
  ENDDO
 ENDDO

 I=0
 DO IROW=1,NROW
  DO ICOL=1,NCOL
  ENDDO
 ENDDO

 CLOSE(IU)

 IF(ALLOCATED(PDELR))DEALLOCATE(PDELR)
 IF(ALLOCATED(PDELC))DEALLOCATE(PDELC)

 END SUBROUTINE

 !###====================================================================
 SUBROUTINE METASWAP_CHECK()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: IROW,ICOL,IU,I,J,N,IOS
 
 !## change surface water into gras; change urban into gras
  DO IROW=1,SIMGRO_NROW
   DO ICOL=1,SIMGRO_NCOL
    SELECT CASE (SIMGRO(ICOL,IROW)%LGN)
     CASE (8,18:21,23:26)
      SIMGRO(ICOL,IROW)%LGN=1
     CASE (22)
      SIMGRO(ICOL,IROW)%LGN=12
     CASE (:0,45:)
      SIMGRO(ICOL,IROW)%LGN=1
    END SELECT
   ENDDO
  ENDDO
! ENDIF

 !## minimale beworteling
 DO IROW=1,SIMGRO_NROW; DO ICOL=1,SIMGRO_NCOL
  IF(SIMGRO(ICOL,IROW)%RZ.LT.10.0)SIMGRO(ICOL,IROW)%RZ=10.0
 ENDDO; ENDDO

 !## minimal nopp-value
 DO IROW=1,SIMGRO_NROW; DO ICOL=1,SIMGRO_NCOL
  SIMGRO(ICOL,IROW)%NOPP=MAX(0.0,SIMGRO(ICOL,IROW)%NOPP)
  !## minimal sopp-value
  SIMGRO(ICOL,IROW)%SOPP=MAX(0.0,SIMGRO(ICOL,IROW)%SOPP)
 ENDDO; ENDDO

 !## bodem 22/23 vertalen naar 9 -> 22 (stedelijk zand?)/23(geen bodem; stad) -> zand
 DO IROW=1,SIMGRO_NROW
  DO ICOL=1,SIMGRO_NCOL
   SELECT CASE (SIMGRO(ICOL,IROW)%BODEM)
    CASE (23,22)
     SIMGRO(ICOL,IROW)%BODEM=9
   END SELECT
   !## kies bodem 22 for lgn stedelijk gebied
   SELECT CASE (SIMGRO(ICOL,IROW)%LGN)
    CASE (18,25)
!     SIMGRO(ICOL,IROW)%BODEM=22
   END SELECT
  ENDDO
 ENDDO

 IF(IARMWP.EQ.0)THEN

  !## turn off beregening whenever layer is nul!
  DO IROW=1,SIMGRO_NROW
   DO ICOL=1,SIMGRO_NCOL
  !## maximal artificial recharge layer is nlay
      SIMGRO(ICOL,IROW)%BER_LAAG=MIN(SIMGRO(ICOL,IROW)%BER_LAAG,SIMGRO_NLAY)
    IF(SIMGRO(ICOL,IROW)%BEREGEN.NE.0.AND.SIMGRO(ICOL,IROW)%BER_LAAG.EQ.0)SIMGRO(ICOL,IROW)%BEREGEN=0
   ENDDO
  ENDDO
 ENDIF

 END SUBROUTINE METASWAP_CHECK


 subroutine storedxc(dxcid,mndxc,ndxc,ilay,irow,icol,id)
 implicit none

 integer, intent(inout) :: ndxc, mndxc
 integer, intent(inout), dimension(mndxc,4) :: dxcid

 integer, intent(in) :: ilay,irow,icol,id

 ndxc = ndxc + 1
 dxcid(ndxc,1) = ilay
 dxcid(ndxc,2) = irow
 dxcid(ndxc,3) = icol
 dxcid(ndxc,4) = id

 end subroutine storedxc


 subroutine writedxc(idxc,dxcid,mndxc,ndxc)
 implicit none

 integer, intent(in) :: idxc, ndxc, mndxc
 integer, intent(in), dimension(mndxc,4) :: dxcid

 character(len=256) :: str
 character(len=256), dimension(4) :: strarr
 integer :: i, j, luncb

 nam%data(idxcflux)%fname  = 'bdgcap'
 nam%data(idxcflux)%cbnlay = dxc%cbnlay
 nam%data(idxcflux)%cblay  = dxc%cblay

 if (dxc%cbnlay.gt.0) then
    nam%data(idxcflux)%active = .true.
    luncb = nam%data(idxcflux)%nunit
 else
    luncb = 0
 end if
      
 write(strarr(1),*) ndxc
 write(strarr(2),*) luncb
 write(str,'(2(a,1x))') (trim(adjustl(strarr(j))),j=1,2)
 write(idxc,'(a)') trim(str)
 write(idxc,'(a)') trim(adjustl(strarr(1)))

 do i = 1, ndxc
    write(strarr(1),*) dxcid(i,1)
    write(strarr(2),*) dxcid(i,2)
    write(strarr(3),*) dxcid(i,3)
    write(strarr(4),*) dxcid(i,4)
    write(str,'(4(a,1x))') (trim(adjustl(strarr(j))),j=1,4)
    write(idxc,'(a)') trim(str)
 end do

 close(idxc)

 end subroutine writedxc


END MODULE MOD_METASWAP
