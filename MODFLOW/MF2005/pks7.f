      SUBROUTINE PKS7AR(IN,MXITER,IGRID)
C     ******************************************************************
C     ALLOCATE STORAGE FOR PKS ARRAYS AND READ PKS DATA
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE PKSMODULE
      USE PKSMPI_MOD, ONLY: MYRANK
#ifdef PKSUNS      
      USE GLOBAL,   ONLY:IAC=>IA, JAC=>JA
      USE GLOBAL,   ONLY:NODES
#endif      
      USE GLOBAL,   ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,IACTCELL
#ifdef PKSOMP      
      USE OMP_LIB
#endif      
C
      IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
      INTEGER, INTENT(IN)    :: IN
      INTEGER, INTENT(INOUT) :: MXITER
      INTEGER, INTENT(IN)    :: IGRID
C       + + + LOCAL DEFINITIONS + + +
      CHARACTER (LEN=16), DIMENSION(0:3) :: cpc
      CHARACTER (LEN=16), DIMENSION(2)   :: cscl
      CHARACTER (LEN=21), DIMENSION(2)   :: cord
      CHARACTER (LEN=16), DIMENSION(2)   :: chopt
      CHARACTER (LEN=16), DIMENSION(3)   :: ccnvgopt

      CHARACTER*200 LINE
C
      INTEGER :: I, J, K, N
      INTEGER :: IC, IEQ
      INTEGER :: IND
      INTEGER :: JCOL
      INTEGER :: ISTART, ISTOP
      INTEGER :: LLOC
      INTEGER :: NODESC, NRC
      INTEGER :: NCORESMAX
      INTEGER :: NUP
      REAL    :: R
      DOUBLE PRECISION :: MASK                                          !JV
      INTEGER, DIMENSION(1) :: IWK
      
      INTEGER :: nlc, nuc, nlmax, numax

      INTEGER, DIMENSION(:), ALLOCATABLE :: iwork0, iwork1
      
      REAL, PARAMETER :: RZERO = 0.0E0
      DOUBLEPRECISION, PARAMETER :: DZERO = 0.0D0
      REAL, PARAMETER :: RONE = 1.0E0
      DOUBLEPRECISION, PARAMETER :: DONE = 1.0D0
C
C       + + + DATA + + +
      DATA cpc       /'JACOBI          ',
     +                'MIC0            ',
     +                'MILU0           ',
     +                'MILUT           '/
      DATA cscl      /'NONE            ',
     +                'DIAGONAL        '/
      DATA cord      /'NONE                 ',
     +                'REVERSE CUTHILL-MCKEE'/
      DATA chopt     /'CPU             ',
     +                'CPU-OPENMP      '/
      DATA ccnvgopt  /'INFINITY NORM   ',
     +                'L2 NORM         ',
     +                'RELATIVE L2 NORM'/
C     ------------------------------------------------------------------
C
C-------ALLOCATE STORAGE FOR SOLVER
      ALLOCATE( ISOLVER )
      ALLOCATE( INNERIT,NPC )
      ALLOCATE( IORD )
      ALLOCATE( ISCL )
      ALLOCATE( NOPT,NCORESM,NCORESV )
      ALLOCATE( NRPROC )                !JV
      ALLOCATE( ICNVGOPT )
      ALLOCATE( IEPFACT )      
      ALLOCATE( NITERC,NIABCGS)
      ALLOCATE( NGMRES,NREST )
      ALLOCATE( NIWC,NWC )
      ALLOCATE( NIAPC,NJAPC,NNZAPC )
      ALLOCATE( NIAPCM,NJAPCM )
      ALLOCATE( HCLOSEPKS, RCLOSEPKS )

      ALLOCATE( NDAMPPKS )
      ALLOCATE( DAMPPKS,DAMPTPKS, RELAXPKS )
      
      ALLOCATE(NRESUP)
      
      ALLOCATE( IFILL   )
      ALLOCATE( NLEVELS )
      ALLOCATE( DROPTOL )

      ALLOCATE( IPKSO, IPKSI )
      
      ALLOCATE( PKST,PKSPCU,PKSPCA )
      ALLOCATE( IPRPKS,MUTPKS,IUNITPKS )

#ifndef PKSUNS      
      ALLOCATE( NNZC,NIAC )
      ALLOCATE( NODEC(NCOL,NROW,NLAY) )
      NODESC = NCOL*NROW*NLAY
      CALL SPKS_ISET(1, NODESC, 0, NODEC)
#else         
      ALLOCATE( CRDUM(1),CCDUM(1),CVDUM(1),HCOFDUM(1),RHSDUM(1))
      NODESC = NODES
      ALLOCATE( IACTCELL(1,1,1) )
      ALLOCATE( NODEC(1,1,1) )
#endif         
      ALLOCATE(EXPLINSYS)
      
      IPKSO     = 0
      IPKSI     = 0
      PKST      = 0.0
      PKSPCU    = 0.0
      PKSPCA    = 0.0
C
C-------PRINT A MESSAGE IDENTIFYING PKS PACKAGE
      WRITE (IOUT,500)
  500 FORMAT (1X,/1X,'PKS -- MODFLOW-2015 PARALLEL KRYLOV SOLVER, ',
     &        'VERSION 0.01, 02/26/2013',
     &        /1X,8X,'INCLUDES SERIAL AND PARALLEL SUPPORT')
C
C-------SET DEFAULTS
      MXITER = 1000
      INNERIT = 50
      ISOLVER = 1
      NPC = 2
      ISCL = 0
      IORD = 0
      HCLOSEPKS = 0.001
      RCLOSEPKS = 0.1  
      NOPT = 1
C     NOPT = 2 ! CARTESIUS
      NCORESM = 1
      NCORESV = 1
      DAMPPKS = RONE
      DAMPTPKS = RONE
      RELAXPKS = RONE
      NRESUP = -999
      IFILL = 0
      DROPTOL = RZERO
      ICNVGOPT = 0
      IEPFACT = 1      
      IPRPKS = 1
      MUTPKS = 1
      IUNITPKS = IOUT
      EXPLINSYS = .FALSE.
      
      NREST = 30
C
C-------READ OPTIONS AND PRINT COMMENTS
      PKSOPT: DO
         CALL URDCOM(IN,IOUT,LINE)
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
         SELECT CASE (LINE(ISTART:ISTOP))
            CASE ('MXITER')   
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXITER,R,IOUT,IN)
            CASE ('INNERIT')   
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,INNERIT,R,IOUT,IN)
            CASE ('GMRES_RESTART')   
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NREST,R,IOUT,IN)
            CASE ('ISOLVER')   
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISOLVER,R,IOUT,IN)
            CASE ('NPC')   
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPC,R,IOUT,IN)
            CASE ('NOPT')   
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NOPT,R,IOUT,IN)
            CASE ('ISCL')   
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISCL,R,IOUT,IN)
            CASE ('IORD')   
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IORD,R,IOUT,IN)
            CASE ('HCLOSEPKS')
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,HCLOSEPKS,IOUT,IN)
            CASE ('RCLOSEPKS')
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,RCLOSEPKS,IOUT,IN)
            CASE ('DAMP')
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,DAMPPKS,IOUT,IN)
            CASE ('DAMPT')
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,DAMPTPKS,IOUT,IN)
            CASE ('RELAX')
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,RELAXPKS,IOUT,IN)
            CASE ('RESIDUAL_UPDATE')   
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NRESUP,R,IOUT,IN)
            CASE ('IFILL')   
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFILL,R,IOUT,IN)
            CASE ('DROPTOL')
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,DROPTOL,IOUT,IN)
            CASE ('NCORESM')   
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NCORESM,R,IOUT,IN)
            CASE ('NCORESV')   
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NCORESV,R,IOUT,IN)
            CASE ('IPRPKS')   
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IPRPKS,R,IOUT,IN)
            CASE ('MUTPKS')   
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MUTPKS,R,IOUT,IN)
            CASE ('IUNITPKS')   
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUNITPKS,R,IOUT,IN)
            CASE ('EXACT')  
              IEPFACT = 0
            CASE ('L2NORM')
              IF ( ICNVGOPT.NE.0 ) THEN
                WRITE (IOUT,'(//,A)') 'PKS7AR: L2NORM CAN NOT BE '//
     2            'SPECIFIED WITH RELATIVE-L2NORM'
                CALL USTOP('PKS7AR: L2NORM OR RELATIVE-L2NORM OPTION')
              END IF
              ICNVGOPT = 1
            CASE ('RELATIVE-L2NORM')   
              IF ( ICNVGOPT.NE.0 ) THEN
                WRITE (IOUT,'(//,A)') 'PKS7AR: RELATIVE-L2NORM CAN '//
     2            'NOT BE SPECIFIED WITH L2NORM'
                CALL USTOP('PKS7AR: RELATIVE-L2NORM OR L2NORM OPTION')
              END IF
              ICNVGOPT = 2
            CASE ('EXPLINSYS')   
               CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I,R,IOUT,IN)
               IF (I.GT.0) EXPLINSYS = .TRUE.
            CASE('END')
               EXIT PKSOPT
            CASE DEFAULT
               WRITE (IOUT,'(A,A)') 'UNRECOGNIZED PKS OPTION: ', 
     &                         LINE(ISTART:ISTOP)             
         END SELECT    
      END DO PKSOPT
C-------CHECK PRECONDITIONER FLAG
      IF ( NPC.LT.0 .OR. NPC.GT.3 ) THEN
          WRITE (IOUT,'(//,A)') 'PKS7AR: NPC MUST BE >= 0 AND <= 3'
          CALL USTOP('PKS7AR: NPC MUST BE >= 1 AND <= 2')
      END IF
C-------CHECK SCALING FLAG
      IF ( ISCL.NE.0 .AND. ISCL.NE.1 ) THEN
          WRITE (IOUT,'(//,A)') 'PKS7AR: ISCL MUST BE 0 OR 1'
          CALL USTOP('PKS7AR: ISCL MUST BE 0 OR 1')
      END IF
C-------CHECK REODERING FLAG
      IF ( IORD.NE.0 .AND. IORD.NE.1 ) THEN
          WRITE (IOUT,'(//,A)') 'PKS7AR: IORD MUST BE 0 OR 1'
          CALL USTOP('PKS7AR: IORD MUST BE 0 OR 1')
      END IF
C-------SET NOPT BASED ON NCORESM AND NCORESV      
      IF ( NCORESM.GT.1 .OR. NCORESV.GT.1 ) THEN
        NOPT = 2
      END IF
C-------CHECKS FOR NCORESM AND NCORESV IF NOPT = 2
      IF ( NOPT.EQ.2 ) THEN
        IF ( NCORESM.LT.1 ) THEN
            WRITE (IOUT,'(//,A)') 'PKS7AR: NCORESM MUST BE > 0'
            CALL USTOP('PKS7AR: NCORESM MUST BE > 0')
        END IF
        IF ( NCORESV.LT.1 ) THEN
            WRITE (IOUT,'(//,A)') 'PKS7AR: NCORESV MUST BE > 0'
            CALL USTOP('PKS7AR: NCORESV MUST BE > 0')
        END IF
      END IF
      CALL PKS7MPIGETNRPROC(NRPROC)                                     !JV
C
C-------CHECK THAT CORRECT CONVERGENCE OPTION SELECTED IF
C       GMRES IS USED
      IF ( ISOLVER.EQ.3 ) THEN
        IF (ICNVGOPT.NE.1) THEN
          WRITE (IOUT,'(//,A)') 
     &      'PKS7AR: RESETTING ICNVGOPT TO 1 FOR GMRES'
          ICNVGOPT = 1
        END IF
      END IF
C
C-------CHECKS FOR HCLOSEPKS AND RCLOSEPCG
      IF ( HCLOSEPKS.LE.0.0 ) THEN
          WRITE (IOUT,'(//,A)') 'PKS7AR: HCLOSE MUST BE > 0.0'
          CALL USTOP('PKS7AR: HCLOSE MUST BE > 0.0')
      END IF
      IF ( RCLOSEPKS.LE.0.0 ) THEN
          WRITE (IOUT,'(//,A)') 'PKS7AR: RCLOSE MUST BE > 0.0'
          CALL USTOP('PKS7AR: RCLOSE MUST BE > 0.0')
      END IF
C-------CHECKS FOR PRINT OPTIONS      
      IF ( IPRPKS.LT.1 ) IPRPKS = 999
      IF ( MUTPKS.LT.0 .OR. MUTPKS.GT.3 ) THEN
          WRITE (IOUT,'(//,A)') 'PKS7AR: MUTPKS MUST BE 0, 1, 2, OR 3'
          CALL USTOP('PKS7AR: MUTPKS MUST BE 0, 1, 2, OR 3')
      END IF
      IF ( IUNITPKS.LT.1 ) THEN
        IUNITPKS = IOUT
      END IF
C
C-------SET DEFAULT RESIDUAL UPDATE FREQUENCY IF USER DID NOT SPECIFY
      IF ( NRESUP < 0 ) THEN
        IF ( ISOLVER.EQ.1 ) THEN
          NRESUP = 0
        ELSE IF ( ISOLVER.EQ.2 ) THEN
          NRESUP = 5
        ELSE IF ( ISOLVER.EQ.3 ) THEN
          NRESUP = 0
        END IF
      END IF
C
C-------PRINT MXITER,INNERIT,NPC,HCLOSEPCG,RCLOSEPCG,ISOLVER,NPC,NOPT
C-------MUTPCG,DAMPPCG
      IF ( ISOLVER.EQ.1 ) THEN
        WRITE (IOUT,505)
      ELSE IF ( ISOLVER.EQ.2 ) THEN
        WRITE (IOUT,510)
      ELSE IF ( ISOLVER.EQ.3 ) THEN
        WRITE (IOUT,511)
      END IF
  505   FORMAT (1X,/,18X,'SOLUTION BY THE CONJUGATE-GRADIENT METHOD',
     &        /,1X,75('-'))
  510   FORMAT (1X,/,11X,'SOLUTION BY THE BICONJUGATE-GRADIENT ',
     &        'STABILIZED METHOD',/,1X,75('-'))
  511   FORMAT (1X,/,11X,'SOLUTION BY THE GENERALIZED MINIMAL ',
     &        'RESIDUAL METHOD',/,1X,75('-'))
      WRITE (IOUT,515) MXITER,INNERIT,NPC,cpc(NPC),
     &                 ISCL,cscl(ISCL+1),IORD,cord(IORD+1),
     &                 NOPT,chopt(NOPT),
     &                 HCLOSEPKS,RCLOSEPKS,
     &                 ICNVGOPT,ccnvgopt(ICNVGOPT+1),(IEPFACT>0),
     &                 NRESUP,RELAXPKS,DAMPPKS,DAMPTPKS,
     &                 IPRPKS,MUTPKS,IUNITPKS
  515 FORMAT (1X,01X,'MAXIMUM NUMBER OF CALLS TO PKS ROUTINE =',I9,/,
     &        1X,05X,'MAXIMUM ITERATIONS PER CALL TO PKS =',I9,/,
     &        1X,12X,'MATRIX PRECONDITIONING TYPE =',I9,/,
     &        1X,18X,'MATRIX PRECONDITIONER =',4X,A16,/,
     &        1X,20X,'MATRIX SCALING TYPE =',I9,/,
     &        1X,25X,'MATRIX SCALING =',4X,A16,/,
     &        1X,17X,'MATRIX REORDERING TYPE =',I9,/,
     &        1X,22X,'MATRIX REORDERING =',4X,A21,/,
     &        1X,17X,'HARDWARE SOLUTION TYPE =',I9,/,
     &        1X,22X,'HARDWARE SOLUTION =',4X,A16,/,
     &        1X,06X,'HEAD CHANGE CRITERION FOR CLOSURE =',E15.5,/,
     &        1X,02X,'RESIDUAL CHANGE CRITERION FOR CLOSURE =',E15.5,/,
     &        1X,12X,'RESIDUAL CONVERGENCE OPTION =',I9,/,
     &        1X,14X,'RESIDUAL CONVERGENCE NORM =',4X,A16,/,
     &        1X,23X,'INEXACT SOLUTION =',4X,L1,/,
     &        1X,14X,'RESIDUAL UPDATE FREQUENCY =',2X,I9,/,
     &        1X,22X,'RELAXATION FACTOR =',E15.5,/,
     &        1X,10X,'STEADY-STATE DAMPENING FACTOR =',E15.5,/,
     &        1X,13X,'TRANSIENT DAMPENING FACTOR =',E15.5,/,
     &        1X,03X,'SOLVER CONVERGENCE PRINTOUT INTERVAL =',I9,/,
     &        1X,05X,'SOLVER CONVERGENCE PRINTING OPTION =',I9,/,
     &        1X,17X,'PRINTING IS LIMITED(1) OR SUPPRESSED (>1)',/,
     &        1X,11X,'PKS CONVERGENCE OUTPUT UNIT =',I9)
C
C-------WRITE MILUT OPTIONS
      IF ( NPC.EQ.3 ) THEN
        WRITE (IOUT,520) IFILL, DROPTOL
      END IF
  520 FORMAT (1X,07X,'MILUT PRECONDITIONER FILL LEVELS =',I9,/,
     &        1X,19X,'MILUT DROP TOLERENCE =',E15.5,/,
     &        1X,75('-'))
C
C-------WRITE GMRES INFORMATION
      IF ( ISOLVER.EQ.3 ) THEN
        WRITE(IOUT,521) NREST
      END IF
  521 FORMAT (1X,16X,'GMRES RESTART FREQUENCY =',I9)
C
C-------FINISH WRITING PKS OPTION TABLE
      WRITE (IOUT,525)
  525 FORMAT (1X,75('-'))
C
C-------ALLOCATE SPACE FOR SAVING PKS CONVERGENCE INFORMATION
      ALLOCATE( PKSCLEN )
      IF (ISOLVER.NE.3) THEN
      PKSCLEN = MXITER*INNERIT
      ELSE
        PKSCLEN = MXITER*INNERIT !*INNERIT
      END IF
      ALLOCATE( PKSIT(PKSCLEN) )
      ALLOCATE( PKSHMXLOC(3,PKSCLEN) )
      ALLOCATE( PKSHMX(PKSCLEN) )
      ALLOCATE( PKSRMXLOC(3,PKSCLEN) )
      ALLOCATE( PKSRMX(PKSCLEN) )
      ALLOCATE( PKSL2NORM(PKSCLEN) )
      ALLOCATE( PKSRL2NORM(PKSCLEN) )
C
C-------ALLOCATE HPKS IF NEEDED FOR SOLUTION DAMPENING
      IF ( DAMPPKS.NE.RONE .OR. DAMPTPKS.NE.RONE ) THEN
        NDAMPPKS = NODESC 
      ELSE
        NDAMPPKS = 1
      END IF
      ALLOCATE ( HPKS(NDAMPPKS) )
C
C-------INITIALIZE NITERC  
      NITERC = 0
C
C-------CALCULATE NUMBER OF NON-ZERO ENTRIES IN MODEL GRID
#ifndef PKSUNS
      NUP     = 0
      NNZC    = 0
      NIAC    = 0
      NIABCGS = 0
      NGMRES  = 0
      NIWC    = 0
      NWC     = 0
      IC      = 0
      ieq     = 0
      NRC     = NROW * NCOL
      DO K = 1, NLAY
        DO I = 1, NROW
          DO J = 1, NCOL
            IC = IC + 1
            IF ( IACTCELL(J,I,K).GT.0 ) THEN
              NIAC = NIAC + 1
              NNZC = NNZC + 1
              ieq  = ieq  + 1
              NODEC(J,I,K) = ieq
C               TOP FACE
              IF ( K.GT.1 ) THEN
                IF ( IACTCELL(J,I,K-1).GT.0 ) NNZC = NNZC + 1
              END IF
C               UPPER FACE
              IF ( I.GT.1 ) THEN
                IF ( IACTCELL(J,I-1,K).GT.0 ) NNZC = NNZC + 1
              END IF
C               LEFT FACE
              IF ( J.GT.1 ) THEN
                IF ( IACTCELL(J-1,I,K).GT.0 ) NNZC = NNZC + 1
              END IF
C               RIGHT FACE
              IF ( J.LT.NCOL ) THEN
                IF ( IACTCELL(J+1,I,K).GT.0 ) NNZC = NNZC + 1
              END IF
C               LOWER FACE
              IF ( I.LT.NROW ) THEN
                IF ( IACTCELL(J,I+1,K).GT.0 ) NNZC = NNZC + 1
              END IF
C               BOTTOM FACE
              IF ( K.LT.NLAY ) THEN
                IF ( IACTCELL(J,I,K+1).GT.0 ) NNZC = NNZC + 1
              END IF
            END IF
          END DO
        END DO
      END DO
#endif
C
C-------CHECK IF THERE ARE NO SUBDOMAINS CONTAINING ZERO ACTIVE CELLS
      IWK(1) = NIAC
      IF(NRPROC.GT.1)THEN
        CALL PKS7MPIWRPALLMINI(IWK,1)
      END IF  
      IF(IWK(1).EQ.0)THEN
        IF(NIAC.EQ.0)THEN
          WRITE(*,'(1X,A,I3.3,1X,A)') 
     1      'Error P', MYRANK, ': no active cells (NIAC=0)' 
        END IF   
        CALL PKS7MPIWRPFINALIZE()
        CALL USTOP(' ')           
      END IF
C
C-------ALLOCATE AND INITIALIZE COMPRESSED ROW STORAGE VECTORS
C       COEFFICIENT MATRIX
      ALLOCATE(IXMAP(NIAC))
      
#ifndef PKSUNS      
      ALLOCATE(AC(NNZC))
      ALLOCATE(IAC(NIAC+1),JAC(NNZC))
      DO n = 1, NIAC
         IXMAP(n) = 0
      END DO 
#else         
      CALL PKS7MPISETIXMAP(IXMAP,NIAC)
      ALLOCATE(AC(1))
#endif         
C       WORKING SOLUTION VECTORS
      ALLOCATE(A0(NNZC))
      ALLOCATE(IA0(NIAC+1),JA0(NNZC))
C       ALLOCATE WORKING VECTORS FOR PKS SOLVER      
#ifndef PKSUNS      
      ALLOCATE(BC(NIAC))
      DO n = 1, NIAC
         BC(n) = 0.0D0
      END DO   
#else         
      ALLOCATE(BC(1))
#endif         
      ALLOCATE(XC(NIAC))
      ALLOCATE(DC(NIAC),PC(NIAC))
      ALLOCATE(QC(NIAC),ZC(NIAC))
C       INITIALIZE PCG WORKING ARRAYS
#ifndef PKSUNS      
      DO n = 1, NNZC
        AC(n)  = 0.0D0
        JAC(n) = 0
      END DO
      DO n = 1, NIAC+1
        IAC(n) = 0
      END DO
#endif         
      DO n = 1, NIAC
        XC(n)    = 0.0D0
C         WORKING ARRAYS
        DC(n)    = 0.0D0
        PC(n)    = 0.0D0
        QC(n)    = 0.0D0
        ZC(n)    = 0.0D0
      END DO
C
      IF (ISOLVER.EQ.2) THEN
        NIABCGS = NIAC
      ELSE
        NIABCGS = 1
      END IF
      ALLOCATE( VC(NIABCGS) )
      ALLOCATE( DTILC(NIABCGS) )
      ALLOCATE( PHATC(NIABCGS) )
      ALLOCATE( DHATC(NIABCGS) )
      DO n = 1, NIABCGS
        VC(n) = 0.0D0
        DTILC(n) = 0.0D0
        PHATC(n) = 0.0D0
        DHATC(n) = 0.0D0
      END DO
C
      IF (ISOLVER.EQ.3) THEN
        NGMRES = NIAC
      ELSE
        NGMRES = 1
        NREST = 1
      END IF
      ALLOCATE( CSG(NREST) )
      ALLOCATE( SNG(NREST) )
      ALLOCATE( SG(NREST+1) )
      ALLOCATE( YG(NREST+1) )
      ALLOCATE( HG(NREST+1, NREST) )
      ALLOCATE( VG(NGMRES, NREST+1) )
      DO n = 1, NREST
        CSG(n) = 0.0D0
        SNG(n) = 0.0D0
      END DO
      DO n = 1, NREST+1
        SG(n) = 0.0D0
        YG(n) = 0.0D0
        DO j = 1, NREST
          HG(n, j) = 0.0D0
        END DO
      END DO
      DO n = 1, NGMRES
        DO j = 1, NREST+1
          VG(n, j) = 0.0D0
        END DO
      END DO
C    
C       ALLOCATE SPACE FOR MIC0, MILU0, MILUT NON-ZERO ROW ENTRY VECTOR
      IF ( NPC.EQ.0 ) THEN
        NIWC = 1
        NWC  = 1
      ELSE IF ( NPC.EQ.1 .OR. NPC.EQ.2 ) THEN
        NIWC = NIAC
        NWC  = NIAC
      ELSE IF ( NPC.EQ.3 ) THEN
        NIWC = 2 * NIAC
        NWC  = NIAC + 1
      ELSE
        NIWC = 1
        NWC  = 1
      END IF
      ALLOCATE(IWC(NIWC))
      ALLOCATE(WC(NWC))
C       INITIALIZE ILU0 AND MILU0 PRECONDITIONER WORKING VECTOR      
      DO n = 1, NIWC
        IWC(n)   = 0
      END DO
      DO n = 1, NWC
        WC(n) = DZERO
      END DO
C       ALLOCATE DIAGONAL SCALING VECTOR
      ALLOCATE( SCL(NIAC)  )
      ALLOCATE( SCLI(NIAC) )
C       INITIALIZE DIAGONAL SCALING VECTOR
      DO n = 1, NIAC
        SCL(n)               = 1.0D0
        SCLI(n)              = 1.0D0
      END DO
C
C-------FILL IA AND JA
#ifndef PKSUNS      
      IND = 0
      IC  = 0
      ieq = 0
      IAJALAY: DO K = 1, NLAY
        IAJAROW: DO I = 1, NROW
          IAJACOL: DO J = 1, NCOL
            IND = IND + 1
            IF ( IACTCELL(J,I,K).GT.0 ) THEN
              IC = IC + 1
              ieq = ieq + 1
              IAC(ieq) = IC
              CALL PKS7MPIMASKBOUND( MASK,J,I,K,NCOL,NROW,NLAY )     !JV
              IF (MASK.GT.0.5D0) THEN                                !JV
                IXMAP(ieq) = IND                                     !JV
              ELSEIF (MASK.LT.-0.5D0) THEN                           !JV
                IXMAP(ieq) = -NODESC-IND                             !JV
              ELSE                                                   !JV
                IXMAP(ieq) = -IND                                    !JV
              END IF                                                 !JV

              JAC(IC) = NODEC(J,I,K)
C               TOP FACE
              IF ( K.GT.1 ) THEN
                IF ( IACTCELL(J,I,K-1).GT.0 ) THEN
                  IC = IC + 1
                  JAC(IC) = NODEC(J,I,K-1)
                END IF
              END IF
C               UPPER FACE
              IF ( I.GT.1 ) THEN
                IF ( IACTCELL(J,I-1,K).GT.0 ) THEN
                  IC = IC + 1
                  JAC(IC) = NODEC(J,I-1,K)
                END IF
              END IF
C               LEFT FACE
              IF ( J.GT.1 ) THEN
                IF ( IACTCELL(J-1,I,K).GT.0 ) THEN
                  IC = IC + 1
                  JAC(IC)  = NODEC(J-1,I,K)
                END IF
              END IF
C               RIGHT FACE
              IF ( J.LT.NCOL ) THEN
                IF ( IACTCELL(J+1,I,K).GT.0 ) THEN
                  IC = IC + 1
                  JAC(IC) = NODEC(J+1,I,K)
                END IF
              END IF
C               LOWER FACE
              IF ( I.LT.NROW ) THEN
                IF ( IACTCELL(J,I+1,K).GT.0 ) THEN
                  IC = IC + 1
                  JAC(IC) = NODEC(J,I+1,K)
                END IF
              END IF
C               BOTTOM FACE
              IF ( K.LT.NLAY ) THEN
                IF ( IACTCELL(J,I,K+1).GT.0 ) THEN
                  IC = IC + 1
                  JAC(IC) = NODEC(J,I,K+1)
                END IF
              END IF
            END IF
          END DO IAJACOL
        END DO IAJAROW
      END DO IAJALAY
C
C-------SET LAST POSITION IN IAC
      IAC(NIAC+1) = IC + 1   
#endif
C
C-------DETERMINE nlmax AND nlmin
      nlmax = 0
      numax = 0
      DO N = 1, NIAC
        nlc = 0
        nuc = 0
        DO I = IAC(N), IAC(N+1)-1
          IF ( JAC(I).LT.N ) nlc = nlc + 1
          IF ( JAC(I).GT.N ) nuc = nuc + 1
        END DO
C         DETERMINE IF nlc OR nuc EXCEED nlmax OR numax
        IF ( nlc.GT.nlmax ) nlmax = nlc
        IF ( nuc.GT.numax ) numax = nuc
      END DO
C
C-------SET NLEVELS BASED ON MAX OF nlmax AND numax
      IF ( NPC.EQ.3 ) THEN
        NLEVELS = MAX( nlmax, numax ) + IFILL
        WRITE (IOUT,530) nlmax, numax, NLEVELS
      END IF
  530 FORMAT (1X,08X,'MAXIMUM NUMBER OF LOWER ENTRIES =',I9,/,
     &        1X,08X,'MAXIMUM NUMBER OF UPPER ENTRIES =',I9,/,
     &        1X,17X,'NUMBER OF MILUT LEVELS =',I9,/,
     &        1X,21X,'DIAGONAL NOT INCLUDED IN MILUT LEVELS',/,
     &        1X,75('-'),/)
C
C-------REORDER IF USING MILUT
      ALLOCATE( NIARO, NNZRO )
      IF ( IORD.NE.0 ) THEN
        NIARO = NIAC
        NNZRO = NNZC
      ELSE
        NIARO = 1
        NNZRO = 1
      END IF
      ALLOCATE( IARO(NIARO+1) )
      ALLOCATE( JARO(NNZRO) )
      ALLOCATE( ARO(NNZRO) )
      ALLOCATE( LORDER(NIARO) )
      ALLOCATE( IORDER(NIARO) )
C-------INITIALIZE REORDERING VECTORS
      DO n = 1, NIARO
        LORDER(n) = 0
        IORDER(n) = 0
      END DO
      DO n = 1, NIARO + 1
        IARO(n) = 0
      END DO
      DO n = 1, NNZRO
        JARO(n) = 0
        ARO(n)  = DZERO
      END DO
C
C-------GENERATE REORDERING VECTORS      
      IF ( IORD.NE.0 ) THEN
        ALLOCATE ( iwork0(NIAC)  )
        ALLOCATE ( iwork1(NIAC) )
        CALL GENRCM(NIAC, NNZC, IAC, JAC,
     2              LORDER, iwork0, iwork1 )
C
C         GENERATE INVERSE OF LORDER
        DO i = 1, NIARO
          IORDER( LORDER(i) ) = i
        END DO
        DO i = 1, NIAC, 6
          WRITE (IOUT,2030) 'ORIGINAL NODE      :',
     2                      (j,j=i,MIN(i+5,NIAC))
          WRITE (IOUT,2040)
          WRITE (IOUT,2030) 'REORDERED INDEX    :',
     2                      (LORDER(j),j=i,MIN(i+5,NIAC))
          WRITE (IOUT,2030) 'REORDERED NODE     :',
     2                      (IORDER(j),j=i,MIN(i+5,NIAC))
          WRITE (IOUT,2050)
         END DO
C         DEALLOCATE TEMPORARY STORAGE
        DEALLOCATE ( iwork0, iwork1 )
      END IF
2030  FORMAT(1X,A20,1X,6(I6,1X))
2040  FORMAT(1X,20('-'),1X,6(6('-'),1X))
2050  FORMAT(1X,62('-'),/)
C
C-------DETERMINE SIZE OF PRECONDITIONER IAPC AND JAPC
      NIAPC  = NIAC
      NJAPC  = NNZC
      NNZAPC = NNZC
      NIAPCM = 1
      NJAPCM = 1
C       JACOBI      
      IF ( NPC.EQ.0 ) THEN
        NIAPC  = 1
        NJAPC  = 1
        NNZAPC = NNZC
C       MILUT        
      ELSE IF ( NPC.EQ.1 ) THEN
        NIAPCM = NIAPC
        NJAPCM = NJAPC
C       MILUT        
      ELSE IF ( NPC.EQ.3 ) THEN
        NIAPC  = NIAC
        NJAPC  = NIAC * ( ( 2 * NLEVELS + 1 ) )
        NNZAPC = NJAPC
      END IF
C
C-------ALLOCATE AND INITIALIZE COMPRESSED ROW STORAGE VECTORS
C       PRECONDITIONER MATRIX
      ALLOCATE(IAPC(NIAPC+1))
      ALLOCATE(JAPC(NJAPC))
      ALLOCATE(APC(NNZAPC))
      ALLOCATE(IAPCM(NIAPCM+1))
      ALLOCATE(JAPCM(NJAPCM))
C
C-------GENERATE IAPC AND JAPC FOR MIC0 AND MILU0
      IF ( NPC.EQ.1 .OR. NPC.EQ.2 ) THEN
        CALL SPKS_PCCRS(NIAC,NNZC,IAC,JAC,
     2                  NIAPC,NJAPC,IAPC,JAPC)
C         SAVE FULL IAPC AND JAPC
        IF ( NPC.EQ.1 ) THEN
          CALL SPKS_ICOPY(NCORESV, NIAPCM+1, IAPC, IAPCM)
          CALL SPKS_ICOPY(NCORESV, NJAPCM, JAPC, JAPCM)
        END IF
      END IF
C
C-------INITIALIZE APC      
      DO n = 1, NNZAPC
        APC(n) = 0.0D0
      END DO
C
C-------DETERMINE IF SPECIFIED NUMBER OF THREADS 
C       EXCEEDS THE MAXIMUM NUMBER OF THREADS AVAILABLE
C       MINUS ONE. IF SO, REDUCE TO THE MAXIMUM.
      IF ( NOPT.EQ.2 ) THEN
#ifdef PKSOMP          
        NCORESMAX = OMP_GET_MAX_THREADS() - 1
#else
        NCORESMAX = 1
#endif
        NCORESM   = MAX(1, MIN( NCORESM, NCORESMAX ))
        NCORESV   = MAX(1, MIN( NCORESV, NCORESMAX ))
C
C        NCORESM = OMP_GET_MAX_THREADS() ! CARTESIUS
C        NCORESV = NCORESM ! CARTESIUS
C
C---------WRITE SUMMARY OF THREADS BEING USED FOR OPEN MP 
C         SMV AND VECTOR OPERATIONS
        WRITE (IOUT,575) NCORESV, NCORESM
C
C---------SET MAXIMUM NUMBER OF THREADS
        NCORESMAX = MAX( NCORESM, NCORESV )
      END IF
C      
  570 FORMAT (/1X,75('-'),
     &        /34X,'OPEN MP',
     &        /23X,'AVERAGE TIME FOR',1X,I3,1X,'OPERATIONS',
     &        /47X,'TIME',
     &        /12X,'OPERATION',22X,'MILLISECONDS',6X,'SPEEDUP',
     &        /1X,75('-')
     &        /12X,' SERIAL VECTOR COPY TIME      :',G15.7,
     &        /12X,' OPENMP VECTOR COPY TIME      :',G15.7,1X,F10.3,
     &        /12X,' SERIAL DOT PRODUCT TIME      :',G15.7,
     &        /12X,' OPENMP DOT PRODUCT TIME      :',G15.7,1X,F10.3,
     &        /12X,' SERIAL SMV PRODUCT TIME      :',G15.7,
     &        /12X,' OPENMP SMV PRODUCT TIME      :',G15.7,1X,F10.3,
     &        /1X,75('.'),
     &        /12X,' NUMBER OF ACTIVE CELLS       :',I10,
     &        /12X,' NUMBER OF NON-ZERO ENTRIES   :',I10,
     &        /1X,75('-'))
  575 FORMAT (/1X,75('-'),
     &        /19X,' NUMBER OF OPENMP   V THREADS :',I5,
     &        /19X,' NUMBER OF OPENMP SMV THREADS :',I5,
     &        /1X,75('-'))

C
C-------SET POINTERS FOR GRID
      CALL PKS7PSV(IGRID)
C
C-------RETURN
      RETURN
      END SUBROUTINE PKS7AR
      
      SUBROUTINE PKS7AP(HNEW,IBOUND,IACTCELL,CR,CC,CV,HCOF,RHS,
     &                 ICNVG,NSTP,KSTP,KPER,MXITER,KITER,
     &                 NCOL,NROW,NLAY,NODES,HNOFLO,IOUT,
     &                 ISOLVER,NPC,NCORESM,NCORESV,ICNVGOPT,IEPFACT,
     &                 NRPROC,                                          !JV
     &                 NITER,ITER1,NNZC,NIAC,NIABCGS,
     &                 NGMRES, NREST,
     &                 NIAPC,NJAPC,NNZAPC,NIAPCM,NJAPCM,
     &                 NIWC,NWC,
     &                 HCLOSE,RCLOSE,
     &                 ISS,NDAMP,DAMP,DAMPT,HPKS,
     &                 NRESUP,RELAX,IFILL,NLEVELS,DROPTOL,
     &                 IPKSO,IPKSI,PKST,PKSPCU,PKSPCA,
     &                 IPRPKS,MUTPKS,IUNITPKS,
     &                 PKSCLEN,PKSIT,
     &                 PKSHMXLOC,PKSHMX,PKSRMXLOC,PKSRMX,
     &                 PKSL2NORM,PKSRL2NORM,
     &                 NODEC,BC,XC,AC,IAC,JAC,IXMAP,
     &                 IORD,NIARO,NNZRO,LORDER,IORDER,IARO,JARO,ARO,
     &                 APC,IAPC,JAPC,IAPCM,JAPCM,
     &                 IWC,WC,DC,ZC,PC,QC,VC,
     &                 DTILC,PHATC,DHATC,
     &                 CSG,SNG,SG,YG,HG,VG,
     &                 ISCL,SCL,SCLI,
     &                 A0,IA0,JA0,EXPLINSYS)
C
C     ******************************************************************
C     SOLUTION BY THE CONJUGATE GRADIENT METHOD -
C                                          UP TO ITER1 ITERATIONS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
cDEBUG
      use pksmpi_mod, only: myrank, l2gnod

      IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
      DOUBLEPRECISION, DIMENSION(NODES), INTENT(INOUT)   :: HNEW
      INTEGER, DIMENSION(NODES), INTENT(INOUT)           :: IBOUND
      INTEGER, DIMENSION(*), INTENT(INOUT)               :: IACTCELL
      REAL, DIMENSION(*), INTENT(INOUT)                  :: CR
      REAL, DIMENSION(*), INTENT(INOUT)                  :: CC
      REAL, DIMENSION(*), INTENT(INOUT)                  :: CV
      REAL, DIMENSION(*), INTENT(IN)                     :: HCOF
      REAL, DIMENSION(*), INTENT(IN)                     :: RHS
      INTEGER, INTENT(INOUT)                             :: ICNVG
      INTEGER, INTENT(IN)                                :: NSTP
      INTEGER, INTENT(IN)                                :: KSTP
      INTEGER, INTENT(IN)                                :: KPER
      INTEGER, INTENT(IN)                                :: MXITER
      INTEGER, INTENT(IN)                                :: KITER
      INTEGER, INTENT(IN)                                :: NCOL
      INTEGER, INTENT(IN)                                :: NROW
      INTEGER, INTENT(IN)                                :: NLAY
      INTEGER, INTENT(IN)                                :: NODES
      REAL, INTENT(IN)                                   :: HNOFLO
      INTEGER, INTENT(IN)                                :: IOUT
      INTEGER, INTENT(IN)                                :: ISOLVER
      INTEGER, INTENT(IN)                                :: NPC
      INTEGER, INTENT(IN)                                :: NCORESM
      INTEGER, INTENT(IN)                                :: NCORESV
      INTEGER, INTENT(IN)                                :: ICNVGOPT
      INTEGER, INTENT(IN)                                :: IEPFACT
      INTEGER, INTENT(IN)                                :: NRPROC      !JV
      INTEGER, INTENT(IN)                                :: NITER
      INTEGER, INTENT(INOUT)                             :: ITER1
      INTEGER, INTENT(IN)                                :: NNZC
      INTEGER, INTENT(IN)                                :: NIAC
      INTEGER, INTENT(IN)                                :: NIABCGS
      INTEGER, INTENT(IN)                                :: NGMRES
      INTEGER, INTENT(IN)                                :: NREST
      INTEGER, INTENT(IN)                                :: NIAPC
      INTEGER, INTENT(IN)                                :: NJAPC
      INTEGER, INTENT(IN)                                :: NNZAPC
      INTEGER, INTENT(IN)                                :: NIAPCM
      INTEGER, INTENT(IN)                                :: NJAPCM
      INTEGER, INTENT(IN)                                :: NIWC
      INTEGER, INTENT(IN)                                :: NWC
      REAL, INTENT(IN)                                   :: HCLOSE
      REAL, INTENT(IN)                                   :: RCLOSE
      INTEGER, INTENT(IN)                                :: ISS
      INTEGER, INTENT(IN)                                :: NDAMP
      REAL, INTENT(IN)                                   :: DAMP
      REAL, INTENT(IN)                                   :: DAMPT
      DOUBLEPRECISION, DIMENSION(NDAMP), INTENT(INOUT)   :: HPKS
      INTEGER, INTENT(IN)                                :: NRESUP
C       MILUT PRECONDITIONER
      REAL, INTENT(IN)                                   :: RELAX
      INTEGER, INTENT(IN)                                :: IFILL
      INTEGER, INTENT(IN)                                :: NLEVELS
      REAL, INTENT(IN)                                   :: DROPTOL
C       REORDERING
      INTEGER, INTENT(IN)                                :: IORD
      INTEGER, INTENT(IN)                                :: NIARO
      INTEGER, INTENT(IN)                                :: NNZRO
      INTEGER, DIMENSION(NIARO), INTENT(IN)              :: LORDER
      INTEGER, DIMENSION(NIARO), INTENT(IN)              :: IORDER
      INTEGER, DIMENSION(NIARO+1), INTENT(INOUT)         :: IARO
      INTEGER, DIMENSION(NNZRO), INTENT(INOUT)           :: JARO
      DOUBLEPRECISION, DIMENSION(NNZRO), INTENT(INOUT)   :: ARO
C       CONVERGENCE AND TIMING VARIABLES
      INTEGER, INTENT(INOUT)                             :: IPKSO
      INTEGER, INTENT(INOUT)                             :: IPKSI
      REAL, INTENT(INOUT)                                :: PKST
      REAL, INTENT(INOUT)                                :: PKSPCU
      REAL, INTENT(INOUT)                                :: PKSPCA
      INTEGER, INTENT(INOUT)                             :: IPRPKS
      INTEGER, INTENT(INOUT)                             :: MUTPKS
      INTEGER, INTENT(INOUT)                             :: IUNITPKS
      INTEGER, INTENT(INOUT)                             :: PKSCLEN
      INTEGER, DIMENSION(PKSCLEN), INTENT(INOUT)        :: PKSIT
      INTEGER, DIMENSION(3,PKSCLEN), INTENT(INOUT)      :: PKSHMXLOC
      REAL, DIMENSION(PKSCLEN), INTENT(INOUT)           :: PKSHMX
      INTEGER, DIMENSION(3,PKSCLEN), INTENT(INOUT)      :: PKSRMXLOC
      REAL, DIMENSION(PKSCLEN), INTENT(INOUT)           :: PKSRMX
      REAL, DIMENSION(PKSCLEN), INTENT(INOUT)           :: PKSL2NORM
      REAL, DIMENSION(PKSCLEN), INTENT(INOUT)           :: PKSRL2NORM
C       SOLVER VARIABLES
      INTEGER, DIMENSION(*), INTENT(INOUT)               :: NODEC
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)    :: BC
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)    :: XC
      DOUBLEPRECISION, DIMENSION(NNZC), INTENT(INOUT)    :: AC
      INTEGER, DIMENSION(NIAC+1), INTENT(IN)             :: IAC
      INTEGER, DIMENSION(NNZC), INTENT(IN)               :: JAC
      INTEGER, DIMENSION(NIAC), INTENT(IN)               :: IXMAP
C       PRECONDITIONER
      DOUBLEPRECISION, DIMENSION(NNZAPC), INTENT(INOUT)  :: APC
      INTEGER, DIMENSION(NIAPC+1), INTENT(INOUT)         :: IAPC
      INTEGER, DIMENSION(NJAPC), INTENT(INOUT)           :: JAPC
      INTEGER, DIMENSION(NIAPCM+1), INTENT(IN)           :: IAPCM
      INTEGER, DIMENSION(NJAPCM), INTENT(IN)             :: JAPCM
C       WORKING ARRAYS
      INTEGER, DIMENSION(NIWC), INTENT(INOUT)            :: IWC
      DOUBLEPRECISION, DIMENSION(NWC), INTENT(INOUT)     :: WC
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)    :: DC
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)    :: ZC
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)    :: PC
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)    :: QC
      DOUBLEPRECISION, DIMENSION(NIABCGS), INTENT(INOUT) :: VC
      DOUBLEPRECISION, DIMENSION(NIABCGS), INTENT(INOUT) :: DTILC
      DOUBLEPRECISION, DIMENSION(NIABCGS), INTENT(INOUT) :: PHATC
      DOUBLEPRECISION, DIMENSION(NIABCGS), INTENT(INOUT) :: DHATC
C       GMRES WORKING ARRAYS
      DOUBLEPRECISION, DIMENSION(NREST), INTENT(INOUT)          :: CSG
      DOUBLEPRECISION, DIMENSION(NREST), INTENT(INOUT)          :: SNG
      DOUBLEPRECISION, DIMENSION(NREST+1), INTENT(INOUT)        :: SG
      DOUBLEPRECISION, DIMENSION(NREST+1), INTENT(INOUT)        :: YG
      DOUBLEPRECISION, DIMENSION(NREST+1,NREST), INTENT(INOUT)  :: HG
      DOUBLEPRECISION, DIMENSION(NGMRES,NREST+1), INTENT(INOUT) :: VG
C       DIAGONAL SCALING VECTOR
      INTEGER, INTENT(IN) :: ISCL
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)    :: SCL
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)    :: SCLI
C       POINTERS FOR USE WITH BOTH ORIGINAL AND RCM ORDERINGS
      DOUBLEPRECISION, DIMENSION(NNZC)                   :: A0
      INTEGER,         DIMENSION(NIAC+1)                 :: IA0
      INTEGER,         DIMENSION(NNZC)                   :: JA0
      LOGICAL, INTENT(IN)                                :: EXPLINSYS  
C     + + + LOCAL DEFINITIONS + + +
      DOUBLEPRECISION, PARAMETER :: DZERO = 0.0D0
      DOUBLEPRECISION, PARAMETER :: DONE  = 1.0D0
      INTEGER :: i, j, k, n, m                                          !JV
      INTEGER :: iapos
      INTEGER :: ieq
      INTEGER :: nrc
      INTEGER :: iinactive
      INTEGER :: iadiag
      INTEGER :: nrn, nrl, ncn, ncl, nln, nll
      INTEGER :: ncf, ncd, nrb, nrh, nls, nlz
      INTEGER :: ICH
      INTEGER :: iiterc
      
      DOUBLEPRECISION :: ddamp

      DOUBLE PRECISION :: rrhs
      DOUBLE PRECISION :: z, b, d, e, f, h, s
      
      REAL :: t0, pct0

      DOUBLEPRECISION :: l2norm
      DOUBLEPRECISION :: l2normbi
      DOUBLEPRECISION :: rmax0
      DOUBLEPRECISION :: l2norm0
      DOUBLEPRECISION :: epfact
      
      INTEGER :: icnvgtab
      
      integer :: iflen
      character(len=10) :: apref,xpref,bpref
      integer, dimension(:), allocatable :: iwrk
      
      LOGICAL :: WRITESTO, PKS7MPIMASTERWRITE
      
C       FUNCTIONS      
      DOUBLEPRECISION :: SPKS_INFNORM
      DOUBLEPRECISION :: SPKS_L2NORM
C
C-------CODE
C
C-------DAMPENING
      IF(ISS.EQ.0) THEN
        ddamp = REAL( DAMPT, 8 )
      ELSE
        ddamp = REAL( DAMP, 8 )
      END IF
C
C-------STORE PREVIOUS HEAD IF MXITER>1      
      IF ( MXITER.GT.1 ) THEN
        IF ( NDAMP.EQ.NODES ) THEN
          CALL SPKS_DCOPY(NCORESV, NODES, HNEW, HPKS)
        END IF
      END IF
C
C-------DISTRIBUTE HNEW
      CALL PKS7MPILXCHD(HNEW, (/1/), 1, 2)                              !JV

#ifndef PKSUNS      
C-------SET LOCAL VARIABLES
      nrc     = NROW * NCOL
      iapos   = 0
      ieq     = 0
C-------INITIALIZE AC TO ZERO
      CALL SPKS_DSET(NCORESV, NNZC, DZERO, AC)
C
C-------LOOP THROUGH ALL NODES IN THE GRID AND SET UP MATRIX EQUATIONS.
C-------NOTE THAT THE FORMULATION OF THESE EQUATIONS IS OPPOSITE IN SIGN
C-------FROM WHAT IS GIVEN IN THE MODFLOW MANUAL SO THAT THE DIAGONAL
C-------AND RHS ARE BOTH POSITIVE (LHS AND RHS ARE MULTIPLIED BY -1)
C-------THIS LOOP STRUCTURE AND INDEXING IS IDENTICAL TO THAT OF PCG2 
C-------AND IS BLATANTLY COPIED FROM HILL, 1990.
      DO k = 1, NLAY
         DO i = 1, NROW
            DO j = 1, NCOL
               n = j + (i-1) * NCOL + (k-1) * nrc  
               IF (IBOUND(N).EQ.0) THEN
                  CC(N) = 0.
                  CR(N) = 0.
                  IF (N.LE.(NODES-NRC)) CV(N) = 0.
                  IF (N.GT.1) CR(N-1) = 0.
                  IF (N.GT.NCOL) CC(N-NCOL) = 0.
                  IF (N.LE.(NODES-NRC) .AND. N.GT.NRC) CV(N-NRC) = 0.
               END IF   
            END DO
         END DO
      END DO
C
      LFILL: DO k = 1, NLAY
        RFILL: DO i = 1, NROW
          CFILL: DO j = 1, NCOL
C
C-------------CALCULATE 1 DIMENSIONAL SUBSCRIPT OF CURRENT CELL AND
C-------------INITIALIZE MATRIX COEFFICIENTS TO ZERO. CHECK IF CELL IS ACTIVE 
C-------------SKIP COEFFICIENT CALCULATIONS IF CELL IS INACTIVE
            n = j + (i-1) * NCOL + (k-1) * nrc
            e = dzero
            z = dzero
            b = dzero
            d = dzero
            f = dzero
            h = dzero
            s = dzero
            iinactive = 1
            ieq       = NODEC(n)
c            ieq       = NODEC(j+(i-1)*ncol+nrc)
C-------------EVALUATE IF CELL CAN BE ACTIVE
            ISACTIVE: IF( IACTCELL(n).GT.0 ) THEN
              iadiag = IAC(ieq)
              ACTASSMBL: IF( IBOUND(n).GT.0 ) THEN
                iinactive = 0
C
C-----------------CALCULATE 1 DIMENSIONAL SUBSCRIPTS FOR LOCATING THE 6
C-----------------SURROUNDING CELLS
                nrn = n + NCOL
                nrl = n - NCOL
                ncn = n + 1
                ncl = n - 1
                nln = n + nrc
                nll = n - nrc
C
C-----------------CALCULATE 1 DIMENSIONAL SUBSCRIPTS FOR CONDUCTANCE TO THE 6
C-----------------SURROUNDING CELLS.
                ncf = n
                ncd = n - 1
                nrb = n - NCOL
                nrh = n
                nls = n
                nlz = n - nrc
C
C-----------------STORE DOUBLE PRECISION VALUE OF RHS FOR CALCULATION OF RESIDUALS
                rrhs    =  RHS(n)
                BC(ieq) = -rrhs
C
C-----------------GET CONDUCTANCES TO NEIGHBORING CELLS.  
C-----------------ACCUMULATE CONTRIBUTIONS TO DIAGONAL COEFFICIENT. IF NEIGHBOR IS 
C-----------------CONSTANT HEAD, MODIFY RHS AND SET OFF-DIAGONAL COEFFICIENT TO 0
                iapos  = iadiag
C
C                 TOP FACE
C-----------------NEIGHBOR IS 1 LAYER BEHIND
                IF ( k.NE.1 ) THEN
                    ich = 0
                    z = CV(nlz)
                    IF ( IACTCELL(nll).GT.0 ) THEN
                    iapos = iapos + 1
                    IF ( IBOUND(nll).GT.0 ) THEN
                      AC(iapos) = -z
                      ELSE IF ( IBOUND(nll).LT.0 ) THEN
                        ich = 1
                      ELSE
                      z = DZERO
                    END IF
                    ELSE
                      IF( IBOUND(nll).LT.0 ) THEN
                        ich = 1
                        !BC(ieq) = BC(ieq) + z*HNEW(nll) 
                      END IF
                    END IF
                    IF ( ich.EQ.1 ) THEN
                      BC(ieq) = BC(ieq) + z*HNEW(nll) 
                  END IF
                END IF
C
C                 UPPER FACE
C-----------------NEIGHBOR IS 1 ROW BACK
                IF ( i.NE.1 ) THEN
                    ich = 0
                    b = CC(nrb)
                    IF ( IACTCELL(nrl).NE.0 ) THEN
                    iapos = iapos + 1
                    IF( IBOUND(nrl).GT.0 ) THEN
                      AC(iapos) = -b
                      ELSE IF( IBOUND(nrl).LT.0 ) THEN
                        ich = 1
                      ELSE
                      b = dzero
                    END IF 
                    ELSE
                      IF( IBOUND(nrl).LT.0 ) THEN
                        ich = 1
                        !BC(ieq) = BC(ieq) + b*HNEW(nrl) 
                      END IF 
                    END IF
                    IF ( ich.EQ.1 ) THEN
                      BC(ieq) = BC(ieq) + b*HNEW(nrl) 
                  END IF
                END IF
C
C                 LEFT FACE
C-----------------NEIGHBOR IS 1 COLUMN BACK
                IF ( j.NE.1 ) THEN
                    ich = 0
                    d = CR(ncd)
                    IF ( IACTCELL(ncl).NE.0 ) THEN
                    iapos = iapos + 1
                    IF( IBOUND(ncl).GT.0 ) THEN
                      AC(iapos) = -d
                      ELSE IF( IBOUND(ncl).LT.0 ) THEN
                        ich = 1
                      ELSE
                      d = dzero
                    END IF 
                    ELSE
                      IF( IBOUND(ncl).LT.0 ) THEN
                        ich = 1
                        !BC(ieq) = BC(ieq) + d*HNEW(ncl) 
                      END IF 
                    END IF
                    IF ( ich.EQ.1 ) THEN
                      BC(ieq) = BC(ieq) + d*HNEW(ncl) 
                  END IF
                END IF
C
C                 RIGHT FACE
C-----------------NEIGHBOR IS 1 COLUMN AHEAD
                IF ( j.NE.NCOL ) THEN
                    ich = 0
                    f = CR(ncf)
                    IF ( IACTCELL(ncn).NE.0 ) THEN
                    iapos = iapos + 1
                    IF( IBOUND(ncn).GT.0 ) THEN
                      AC(iapos) = -f
                      ELSE IF( IBOUND(ncn).LT.0 ) THEN
                        ich = 1
                      ELSE
                      f = dzero 
                    END IF
                    ELSE
                      IF( IBOUND(ncn).LT.0 ) THEN
                        ich = 1
                        !BC(ieq) = BC(ieq) + f*HNEW(ncn) 
                      END IF
                    END IF
                    IF ( ich.EQ.1 ) THEN
                      BC(ieq) = BC(ieq) + f*HNEW(ncn) 
                  END IF
                END IF
C
C                 LOWER FACE
C-----------------NEIGHBOR IS 1 ROW AHEAD
                IF ( i.NE.NROW ) THEN
                    ich = 0
                    h = CC(nrh)
                    IF ( IACTCELL(nrn).NE.0 ) THEN
                    iapos = iapos + 1
                    IF( IBOUND(nrn).GT.0 ) THEN
                      AC(iapos) = -h
                      ELSE IF( IBOUND(nrn).LT.0 ) THEN
                        ich = 1
                      ELSE
                        h = dzero
                      END IF
                    ELSE
                    IF( IBOUND(nrn).LT.0 ) THEN
                        ich = 1
                        !BC(ieq) = BC(ieq) + h*HNEW(nrn) 
                      END IF 
                    END IF
                    IF ( ich.EQ.1 ) THEN
                      BC(ieq) = BC(ieq) + h*HNEW(nrn) 
                  END IF
                END IF
C
C                 BOTTOM FACE
C-----------------NEIGHBOR IS 1 LAYER AHEAD
                IF ( k.NE.NLAY ) THEN
                    ich = 0
                    s = CV(nls)
                    IF ( IACTCELL(nln).NE.0 ) THEN
                    iapos = iapos + 1
                    IF( IBOUND(nln).GT.0 ) THEN
                      AC(iapos) = -s
                      ELSE IF( IBOUND(nln).LT.0 ) THEN
                        ich = 1
                      ELSE
                        h = dzero
                      END IF
                    ELSE
                    IF( IBOUND(nln).LT.0 ) THEN
                        ich = 1
                        !BC(ieq) = BC(ieq) + s*HNEW(nln) 
                      END IF
                    END IF
                    IF ( ich.EQ.1 ) THEN
                      BC(ieq) = BC(ieq) + s*HNEW(nln) 
                    END IF
                    END IF
C    
C-----------------CHECK IF CELL IS ACTIVE (E + HCOF > 0).
C-----------------IF SURROUNDING CELLS ARE INACTIVE BUT CURRENT CELL IS ACTIVE,
C-----------------AND NO HEAD DEPENDENT BOUNDARY CONDITIONS (HCOF) THEN SET
C-----------------HNEW TO HNOFLO, IBOUND TO 0, AND CHANGE INACTIVE FLAG TO 1
                e = z + b + d + f + h + s
                e = e - HCOF(n)
                IF ( e.EQ.dzero ) THEN
                  HNEW(n)   = HNOFLO
                  IBOUND(n) = 0
                  iinactive = 1
                END IF
C---------------END IBOUND(N) .GT. 0
              END IF ACTASSMBL
C
C---------------IF INACTIVE OR CONSTANT HEAD, SET DIAGONAL TO 1.0, AND ADJUST RHS ACCORDINGLY.  
              IF ( iinactive.EQ.1 ) THEN
                e = DONE
                BC(ieq) = HNEW(n)
              END IF
C
C---------------STORE THE COEFFICENTS OF THE DIAGONAL IN A
              AC(iadiag) = e
C---------------STORE INITIAL GUESS OF HEADS
              XC(ieq) = HNEW(n)     
C-------------END IACTCELL(N) .GT. 0
            END IF ISACTIVE
          END DO CFILL
        END DO RFILL
      END DO LFILL
#endif  
C   
      IF (EXPLINSYS)THEN
C      IF (.TRUE.)THEN
       allocate(iwrk(niac))
#ifndef PKSUNS
        iwrk = 1 ! all active
#else        
        do n=1,niac
           iwrk(n) = ibound(n)    
        end do        
#endif
        apref = 'a'; xpref = 'x'; bpref = 'b'
        iflen = 1; call pks7mpifname(apref,iflen)
        iflen = 1; call pks7mpifname(xpref,iflen)
        iflen = 1; call pks7mpifname(bpref,iflen)
        call export_system(niac,nnzc,bc,xc,ac,iac,jac,iwrk,
     1     trim(apref),trim(xpref),trim(bpref),'csv',
     2     kper,kstp,kiter)
        deallocate(iwrk)
      END IF

C-------START PKS TIMER
      CALL SPKS_TIMER(0,t0,PKST)
C
C-------UPDATE PKS OUTER ITERATION COUNTER
      IPKSO = IPKSO + 1
C
C-------CALCULATE THE L2NORM OF B (RHS)
      l2norm = SPKS_L2NORM(NCORESV,NRPROC,NIAC,BC,IXMAP)
      IF ( l2norm.NE.DZERO ) THEN
        l2normbi = DONE / l2norm
      END IF
C
C-------PERMUTE IAC, JAC, AC, SOLUTION VECTOR (XC), BC, AND IXMAP
      IF ( IORD.NE.0 ) THEN
        CALL DPERM(NIAC,AC,JAC,IAC,ARO,JARO,IARO,LORDER,IWC,1)
        CALL VPERM(NIAC,  XC, LORDER)  
        CALL VPERM(NIAC,  BC, LORDER)
        CALL IVPERM(NIAC, IXMAP, LORDER)
        CALL SPKS_DCOPY(NCORESV, NNZC, ARO, A0)
        CALL SPKS_ICOPY(NCORESV, NIAC+1, IARO, IA0)
        CALL SPKS_ICOPY(NCORESV, NNZC, JARO, JA0)
      ELSE
        CALL SPKS_DCOPY(NCORESV, NNZC, AC, A0)
        CALL SPKS_ICOPY(NCORESV, NIAC+1, IAC, IA0)
        CALL SPKS_ICOPY(NCORESV, NNZC, JAC, JA0)
      END IF
C
C-------SCALE AC, XC, AND BC IF REQUIRED
      IF ( ISCL.NE.0 ) THEN
        CALL SPKS_SCL(1,NNZC,NIAC,A0,XC,BC,SCL,SCLI,IA0,JA0,NODEC)
      END IF
C
C-------UPDATE PRECONDITIONER
      CALL SPKS_TIMER(0,pct0,PKSPCU)
      CALL SPKS_PCU(IOUT,NCORESM,NCORESV,NNZC,NIAC,
     M              NIAPC,NJAPC,NNZAPC,NIAPCM,NJAPCM,NIWC,NWC,
     M              NPC,RELAX,IFILL,NLEVELS,DROPTOL,
     M              A0,IA0,JA0,APC,IAPC,JAPC,IAPCM,JAPCM,
     M              IWC,WC,IXMAP,NODES)   !JV
      CALL SPKS_TIMER(1,pct0,PKSPCU)
C-------CALCULATE INITIAL RESIDUAL
      CALL SPKS_CMATVEC(NCORESM,NIAC,NNZC,IA0,JA0,A0,XC,DC)
      CALL SPKS_AXPY(NCORESV, NIAC, BC, -DONE, DC, DC)
C-------CALCULATE THE INITIAL RMAX and L2NORM
      l2norm0 = DZERO
C---------UNSCALE RESIDUAL AND DETERMINE MAXIMUM INITIAL RESIDUAL AND L2NORM 
      IF ( ISCL.NE.0 ) THEN
        CALL SPKS_DCOPY(NCORESV, NIAC, DC, ZC)
        CALL SPKS_VVMULT(NCORESV,NIAC,ZC,SCLI,ZC)
        rmax0   = SPKS_INFNORM(NCORESV,NIAC,ZC,IXMAP)
        l2norm0 = SPKS_L2NORM(NCORESV,NRPROC,NIAC,ZC,IXMAP)
      ELSE
        rmax0   = SPKS_INFNORM(NCORESV,NIAC,DC,IXMAP)
        l2norm0 = SPKS_L2NORM(NCORESV,NRPROC,NIAC,DC,IXMAP)
      END IF
      IF ( IEPFACT.GT.0 ) THEN
      IF ( KSTP.EQ.1 .AND. KITER.EQ.1 ) THEN
          epfact = 0.01D0
      ELSE
          epfact = 0.10D0
        END IF
      ELSE
        epfact = 1.0D0
      END IF
C-------CALL APPROPRIATE SOLVER
      IF ( ISOLVER.EQ.1 ) THEN
        CALL SPKS_CG(ICNVG,MXITER,KITER,
     &               NPC,NCORESM,NCORESV,
     &               NRPROC,                                            !JV
     &               NITER,ITER1,NNZC,NIAC,
     &               NIAPC,NJAPC,NNZAPC,
     &               ICNVGOPT,l2norm0,l2normbi,epfact,
     &               HCLOSE,RCLOSE,NRESUP,
     &               IPKSI,PKSPCA,
     &               PKSCLEN,PKSIT,
     &               PKSHMXLOC,PKSHMX,
     &               PKSRMXLOC,PKSRMX,PKSL2NORM,PKSRL2NORM,
     &               XC,BC,A0,IA0,JA0,
     $               APC,IAPC,JAPC,
     &               DC,ZC,PC,QC,ISCL,SCL,SCLI,
     &               IXMAP,NODEC,NODES)                                 !JV
      ELSE IF ( ISOLVER.EQ.2 ) THEN
        CALL SPKS_BICGSTAB(ICNVG,MXITER,KITER,
     &                     NPC,NCORESM,NCORESV,
     &                     NRPROC,                                      !JV
     &                     NITER,ITER1,NNZC,NIAC,
     &                     NIAPC,NJAPC,NNZAPC,
     &                     ICNVGOPT,l2norm0,l2normbi,epfact,
     &                     HCLOSE,RCLOSE,NRESUP,
     &                     IPKSI,PKSPCA,
     &                     PKSCLEN,PKSIT,
     &                     PKSHMXLOC,PKSHMX,
     &                     PKSRMXLOC,PKSRMX,PKSL2NORM,PKSRL2NORM,
     &                     XC,BC,A0,IA0,JA0,
     &                     APC,IAPC,JAPC,
     &                     DC,ZC,PC,QC,VC,
     &                     DTILC,PHATC,DHATC,
     &                     ISCL,SCL,SCLI,
     &                     IXMAP,NODEC,NODES)                           !JV
      ELSE IF ( ISOLVER.EQ.3 ) THEN
        epfact = 1.0D0
        CALL SPKS_GMRES(ICNVG,MXITER,KITER,
     &                  NPC,NCORESM,NCORESV,
     &                  NRPROC,                                         !JV
     &                  NITER,ITER1,NNZC,NIAC,NREST,
     &                  NIAPC,NJAPC,NNZAPC,
     &                  ICNVGOPT,l2norm0,l2normbi,epfact,
     &                  HCLOSE,RCLOSE,NRESUP,
     &                  IPKSI,PKSPCA,
     &                  PKSCLEN,PKSIT,
     &                  PKSHMXLOC,PKSHMX,
     &                  PKSRMXLOC,PKSRMX,PKSL2NORM,PKSRL2NORM,
     &                  XC,BC,A0,IA0,JA0,
     &                  APC,IAPC,JAPC,
     &                  DC,ZC,PC,QC,
     &                  CSG,SNG,SG,YG,HG,VG,
     &                  ISCL,SCL,SCLI,
     &                  IXMAP,NODEC,NODES)                              !JV
      END IF
C
C-------UNSCALE XC
      IF ( ISCL.NE.0 ) THEN
        CALL SPKS_SCL(0,NNZC,NIAC,A0,XC,BC,SCL,SCLI,IA0,JA0,NODEC)
      END IF
C
C-------BACK PERMUTE SOLUTION, RHS, AND IXMAP
      IF ( IORD.NE.0 ) THEN
        CALL VPERM(NIAC,  XC, IORDER)  
        CALL VPERM(NIAC,  BC, IORDER)  
        CALL IVPERM(NIAC, IXMAP, IORDER)
      END IF
C
C-------FILL HNEW WITH NEW ESTIMATE
#ifndef PKSUNS
      DO n = 1, NIAC
        M = IXMAP(N)                                                    !JV
        IF (M.LT.-NODES) M = M + NODES                                  !JV
        M = ABS(M)                                                      !JV
        HNEW(M) = XC(n)                                                 !JV
      END DO
#endif
C
C-------DISTRIBUTE UPDATED HNEW
      CALL PKS7MPILXCHD(HNEW, (/1/), 1, 2)                              !JV
C
C-------AT END OF EXTERNAL ITERATION, APPLY DAMP
      IF ( MXITER.GT.1 ) THEN
        IF ( NDAMP.EQ.NODES ) THEN
          DO n = 1, NODES
            IF ( IBOUND(n).LE.0 ) CYCLE
            HNEW(n) = ( DONE - ddamp ) * HPKS(n) + ddamp * HNEW(n)
          END DO
        END IF
      END IF
C
C-------END PKS TIMER
      CALL SPKS_TIMER(1,t0,PKST)
C
C-------WRITE CONVERGENCE RESULTS
      icnvgtab = 0
      IF ( ICNVG.NE.0 .OR. KITER.EQ.MXITER ) THEN
        IF ( MOD(KSTP,IPRPKS).EQ.0 .OR.
     2       KSTP.EQ.NSTP ) icnvgtab = 1
      ELSE IF ( ICNVG.EQ.0 .AND. KITER.EQ.MXITER ) THEN 
        IF ( MUTPKS.EQ.3) icnvgtab = 1
      END IF
      IF ( icnvgtab.NE.0 ) THEN
        CALL SPKS_PCNVG(NCOL,NROW,NLAY,NIAC,IXMAP,
     &                     MUTPKS,IUNITPKS,
     &                     KITER,KSTP,KPER,ITER1,
     &                     PKSCLEN,PKSIT,PKSHMXLOC,PKSHMX,
     &                     PKSRMXLOC,PKSRMX,PKSL2NORM,PKSRL2NORM,
     &                     NODES,NRPROC)   
      END IF
C
C-------IF END OF TIME STEP, PRINT # OF ITERATIONS THIS STEP
      IF ( ICNVG.NE.0 .OR. KITER.EQ.MXITER ) THEN
        WRITE (IOUT,510)
  510   FORMAT (/)
        WRITE (IOUT,515) KITER, KSTP, KPER, ITER1, 
     &                   PKSHMX(ITER1),PKSRMX(ITER1),
     &                   PKSL2NORM(ITER1),
     &                   PKSRL2NORM(ITER1),
     &                   PKST, PKSPCU, PKSPCA
        
       
        WRITESTO = PKS7MPIMASTERWRITE()                                 !JV
        IF (WRITESTO) WRITE(*,*) 'TOTAL ITERATIONS:',ITER1              !JV
        
  515 FORMAT (1X,76('-'),
     &          /,9X,I6,' CALLS TO PKS ROUTINE FOR TIME STEP',I4,
     &          ' IN STRESS PERIOD ',I4,/,9X,I6,' TOTAL ITERATIONS',
     &          /,1X,76('-'),
     &          /,G15.6,' MAXIMUM HEAD CHANGE AT END OF TIMESTEP'
     &          /,G15.6,' MAXIMUM RESIDUAL AT END OF TIMESTEP'
     &          /,G15.6,' MAXIMUM L2-NORM AT END OF TIMESTEP'
     &          /,G15.6,' MAXIMUM RELATIVE L2-NORM AT END OF TIMESTEP'
     &          /,1X,76('-'),
     &          /,G15.6,' SECONDS IN SOLVER AND PRECONDITIONER',
     &          /,G15.6,' SECONDS UPDATING THE PRECONDITIONER',
     &          /,G15.6,' SECONDS APPLYING THE PRECONDITIONER',
     &          /,1X,76('-'))
        ITER1 = 0
C====================================DEBUG===========================
c        call dwriteasc(hnew,1,ncol,nrow,nlay,'PKS_hnew_end',-9999.)    !JV
c        call dwriteasc2d(hnew,ncol,nrow,nlay,'PKS_hnew_end',-9999.)     
c        call pks7mpiwrpfinalize()
c        call ustop(' ') 
C====================================DEBUG===========================


      ENDIF     
C
C-------RETURN
      RETURN
C
      END SUBROUTINE PKS7AP
      
      SUBROUTINE SPKS_PCNVG(NCOL,NROW,NLAY,NIAC,IXMAP,
     &                      MUTPKS,IUNITPKS,
     &                      KITER,KSTP,KPER,ITER1,
     &                      PKSCLEN,PKSIT,PKSHMXLOC,PKSHMX,
     &                      PKSRMXLOC,PKSRMX,PKSL2NORM,PKSRL2NORM,
     &                      NODES,NRPROC) 
C
C     ******************************************************************
C     OUTPUT TABLE OF PKS CONVERGENCE DATA
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
      INTEGER, INTENT(IN)                                :: NCOL
      INTEGER, INTENT(IN)                                :: NROW
      INTEGER, INTENT(IN)                                :: NLAY
      INTEGER, INTENT(IN)                                :: NIAC
      INTEGER, DIMENSION(NIAC), INTENT(IN)               :: IXMAP
      INTEGER, INTENT(IN)                                :: MUTPKS
      INTEGER, INTENT(IN)                                :: IUNITPKS
      INTEGER, INTENT(IN)                                :: KITER
      INTEGER, INTENT(IN)                                :: KSTP
      INTEGER, INTENT(IN)                                :: KPER
      INTEGER, INTENT(IN)                                :: ITER1
      INTEGER, INTENT(IN)                                :: PKSCLEN
      INTEGER, DIMENSION(PKSCLEN), INTENT(IN)           :: PKSIT
      INTEGER, DIMENSION(3,PKSCLEN), INTENT(INOUT)      :: PKSHMXLOC
      REAL, DIMENSION(PKSCLEN), INTENT(IN)              :: PKSHMX
      INTEGER, DIMENSION(3,PKSCLEN), INTENT(INOUT)      :: PKSRMXLOC
      REAL, DIMENSION(PKSCLEN), INTENT(IN)              :: PKSRMX
      REAL, DIMENSION(PKSCLEN), INTENT(IN)              :: PKSL2NORM
      REAL, DIMENSION(PKSCLEN), INTENT(IN)              :: PKSRL2NORM
      INTEGER, INTENT(IN)                               :: NODES
      INTEGER, INTENT(IN)                               :: NRPROC
C     + + + LOCAL DEFINITIONS + + +
      INTEGER :: i, j, k, n
      INTEGER :: node
      INTEGER :: ngrp
      INTEGER :: jmin
      INTEGER :: l1, l2
C     + + + FUNCTIONS + + +
C     + + + FORMATS + + +
C     + + + CODE + + +
C
C-------NOT YET SUPPORTED FOR UNSTRUCTURED
#ifdef PKSUNS
      RETURN
#endif      
C
C-------CONVERT MAXIMUM HEAD CHANGE AND RESICUAL LOCATIONS FROM
C       EQUATION NUMBER (node) TO LAYER, ROW, COLUMN
      DO n = 1, ITER1
C---------MAXIMUM HEAD CHANGE LOCATION        
        node = PKSHMXLOC(1,n)
        IF (NRPROC.GT.1) THEN
          call pks7mpigetgircl(node,j,i,k)  
        ELSE   
          CALL SPKS_NODE2LRC(NLAY,NROW,NCOL,node,k,i,j)
        END IF  
        PKSHMXLOC(1,n) = K
        PKSHMXLOC(2,n) = i
        PKSHMXLOC(3,n) = j
C---------MAXIMUM RESIDUAL LOCATION        
        node = PKSRMXLOC(1,n)
        IF (NRPROC.GT.1) THEN
          call pks7mpigetgircl(node,j,i,k)  
        ELSE   
          CALL SPKS_NODE2LRC(NLAY,NROW,NCOL,node,k,i,j)
        END IF  
        PKSRMXLOC(1,n) = K
        PKSRMXLOC(2,n) = i
        PKSRMXLOC(3,n) = j
      END DO
      
      IF ( MUTPKS.EQ.0 ) THEN
         WRITE(IUNITPKS,5)
5        FORMAT(1X,/1X,'MAXIMUM HEAD CHANGE FOR EACH ITERATION',
     1       ' (1 INDICATES THE FIRST INNER ITERATION):',//
     2       1X,5('   HEAD CHANGE '),/
     3       1X,5('  LAYER,ROW,COL')/1X,75('-'))
         ngrp = ( ITER1 - 1 ) / 5 + 1
         DO k = 1, ngrp
            l1 =( k-1 ) * 5 + 1
            l2 = l1 + 4
            IF ( k.EQ.ngrp ) l2 = ITER1
            IF ( NCOL.LE.999 .AND. NROW.LE.999 ) THEN
              WRITE(IUNITPKS,10) (PKSIT(j),PKSHMX(j),j=l1,l2)
              WRITE(IUNITPKS,11) ((PKSHMXLOC(i,j),i=1,3),j=l1,l2)
10            FORMAT(5(2X,I1,G12.4))
11            FORMAT(1X,5(:'  (',I3,',',I3,',',I3,')'))
            ELSE
              WRITE(IUNITPKS,13) (PKSIT(j),PKSHMX(j),j=l1,l2)
              WRITE(IUNITPKS,14) ((PKSHMXLOC(i,j),i=1,3),j=l1,l2)
13            FORMAT(5(4X,I1,G12.4,2X))
14            FORMAT(1X,5(:'  (',I3,',',I5,',',I5,')'))
            ENDIF
         END DO  
         WRITE(IUNITPKS,25)
25       FORMAT(1X,/1X,'MAXIMUM RESIDUAL FOR EACH ITERATION',
     1       ' (1 INDICATES THE FIRST INNER ITERATION):',//
     2       1X,5('   RESIDUAL    '),/
     3       1X,5('  LAYER,ROW,COL')/1X,75('-'))
         DO k = 1, ngrp
            l1 = ( k - 1 ) * 5 + 1
            l2 = l1 + 4
            IF( k.EQ.ngrp ) l2 = ITER1
            WRITE(IUNITPKS,10) (PKSIT(j),PKSRMX(j),j=l1,l2)
            WRITE(IUNITPKS,11) ((PKSRMXLOC(i,j),i=1,3),j=l1,l2)
         END DO  
         WRITE(IUNITPKS,125)
125      FORMAT(1X,/1X,'L2-NORM FOR EACH ITERATION',
     1       ' (1 INDICATES THE FIRST INNER ITERATION):',//
     2       1X,5('    L2-NORM    '),/1X,75('-'))
         DO k = 1, ngrp
            l1 = ( k - 1 ) * 5 + 1
            l2 = l1 + 4
            IF ( k.EQ.ngrp ) l2 = ITER1
            WRITE(IUNITPKS,10) (PKSIT(j),PKSL2NORM(j),j=l1,l2)
         END DO   
         WRITE(IUNITPKS,126)
126      FORMAT(1X,/1X,'RELATIVE L2-NORM FOR EACH ITERATION',
     1       ' (1 INDICATES THE FIRST INNER ITERATION):',//
     2       1X,5(' REL. L2-NORM  '),/1X,75('-'))
         DO k = 1, ngrp
            l1 = ( k - 1 ) * 5 + 1
            l2 = l1 + 4
            IF ( k.EQ.ngrp ) l2 = ITER1
            WRITE(IUNITPKS,10) (PKSIT(j),PKSRL2NORM(j),j=l1,l2)
         END DO   
         WRITE(IUNITPKS,31)
31       FORMAT(/)
      ELSE
         WRITE(IUNITPKS,35)
35       FORMAT (1X,/1X,'MAXIMUM HEAD CHANGE FOR LAST ITER1 ITERATIONS',
     1          /1X,'(1 INDICATES THE FIRST INNER ITERATION):',//,
     2       1X,5('   HEAD CHANGE '),/
     3       1X,5('  LAYER,ROW,COL')/1X,75('-'))
         jmin = MAX(1,ITER1-ITER1+1)
         ngrp = ( ITER1 - jmin ) / 5 + 1
         DO k = 1, ngrp
            l1 = ( k - 1 ) * 5 + jmin
            l2 = l1 + 4
            IF ( k.EQ.ngrp ) l2 = ITER1
            WRITE(IUNITPKS,10) (PKSIT(j),PKSHMX(j),j=l1,l2)
            WRITE(IUNITPKS,11) ((PKSHMXLOC(i,j),i=1,3),j=l1,l2)
         END DO  
         WRITE(IUNITPKS,45)
45       FORMAT(1X,/1X,'MAXIMUM RESIDUAL FOR LAST ITER1 ITERATIONS',
     1          /1X,'(1 INDICATES THE FIRST INNER ITERATION):',//,
     2       1X,5('   RESIDUAL    '),/
     3       1X,5('  LAYER,ROW,COL')/1X,75('-'))
         DO k = 1, ngrp
            l1 = ( k - 1) * 5 + jmin
            l2 = l1 + 4
            IF ( k.EQ.ngrp ) l2 = ITER1
            WRITE(IUNITPKS,10) (PKSIT(j),PKSRMX(j),j=l1,l2)
            WRITE(IUNITPKS,11) ((PKSRMXLOC(i,j),i=1,3),j=l1,l2)
         END DO  
         WRITE(IUNITPKS,145)
145      FORMAT(1X,/1X,'L2-NORM FOR LAST ITER1 ITERATIONS',
     1          /1X,'(1 INDICATES THE FIRST INNER ITERATION):',//,
     2       1X,5('    L2-NORM    '),/1X,75('-'))
         DO k = 1, ngrp
            l1 = ( k - 1 ) * 5 + jmin
            l2 = l1 + 4
            IF ( k.EQ.ngrp ) l2 = ITER1
            WRITE(IUNITPKS,10) (PKSIT(j),PKSL2NORM(j),j=l1,l2)
         END DO  
         WRITE(IUNITPKS,146)
146      FORMAT(1X,/1X,'RELATIVE L2-NORM FOR LAST ITER1 ITERATIONS',
     1          /1X,'(1 INDICATES THE FIRST INNER ITERATION):',//,
     2       1X,5(' REL. L2-NORM  '),/1X,75('-'))
         DO k = 1, ngrp
            l1 = ( k - 1 ) * 5 + jmin
            l2 = l1 + 4
            IF ( k.EQ.ngrp ) l2 = ITER1
            WRITE(IUNITPKS,10) (PKSIT(j),PKSRL2NORM(j),j=l1,l2)
         END DO  
         WRITE(IUNITPKS,31)
      END IF
C
C-------RETURN
      RETURN
C
      END SUBROUTINE SPKS_PCNVG

     
      SUBROUTINE SPKS_CG(ICNVG,MXITER,KITER,
     &                   NPC,NCORESM,NCORESV,
     &                   NRPROC,                                        !JV
     &                   NITER,ITER1,NNZC,NIAC,
     &                   NIAPC,NJAPC,NNZAPC,
     &                   ICNVGOPT,RMAX0,L2NORMBI,EPFACT,
     &                   HCLOSE,RCLOSE,NRESUP,
     &                   IPKSI,PKSPCA,
     &                   PKSCLEN,PKSIT,PKSHMXLOC,PKSHMX,
     &                   PKSRMXLOC,PKSRMX,PKSL2NORM,PKSRL2NORM,
     &                   XC,BC,AC,IAC,JAC,
     &                   APC,IAPC,JAPC,
     &                   DC,ZC,PC,QC,ISCL,SCL,SCLI,
     &                   IXMAP,NODEC,NODES)                             !JV
C
      use pksmpi_mod, only: myrank

      IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
      INTEGER, INTENT(INOUT)                           :: ICNVG
      INTEGER, INTENT(IN)                              :: MXITER
      INTEGER, INTENT(IN)                              :: KITER
      INTEGER, INTENT(IN)                              :: NPC
      INTEGER, INTENT(IN)                              :: NCORESM
      INTEGER, INTENT(IN)                              :: NCORESV
      INTEGER, INTENT(IN)                              :: NRPROC        !JV
      INTEGER, INTENT(IN)                               :: NITER
      INTEGER, INTENT(INOUT)                            :: ITER1
      INTEGER, INTENT(IN)                               :: NNZC
      INTEGER, INTENT(IN)                               :: NIAC
      INTEGER, INTENT(IN)                               :: NIAPC
      INTEGER, INTENT(IN)                               :: NJAPC
      INTEGER, INTENT(IN)                               :: NNZAPC
      INTEGER, INTENT(IN)                               :: ICNVGOPT
      DOUBLEPRECISION, INTENT(INOUT)                    :: RMAX0
      DOUBLEPRECISION, INTENT(IN)                       :: L2NORMBI
      DOUBLEPRECISION, INTENT(IN)                       :: EPFACT
      REAL, INTENT(IN)                                  :: HCLOSE
      REAL, INTENT(IN)                                  :: RCLOSE
      INTEGER, INTENT(IN)                               :: NRESUP
      INTEGER, INTENT(INOUT)                            :: IPKSI
      REAL, INTENT(INOUT)                               :: PKSPCA
      INTEGER, INTENT(IN)                               :: PKSCLEN
      INTEGER, DIMENSION(PKSCLEN), INTENT(INOUT)       :: PKSIT
      INTEGER, DIMENSION(3,PKSCLEN), INTENT(INOUT)     :: PKSHMXLOC
      REAL, DIMENSION(PKSCLEN), INTENT(INOUT)          :: PKSHMX
      INTEGER, DIMENSION(3,PKSCLEN), INTENT(INOUT)     :: PKSRMXLOC
      REAL, DIMENSION(PKSCLEN), INTENT(INOUT)          :: PKSRMX
      REAL, DIMENSION(PKSCLEN), INTENT(INOUT)          :: PKSL2NORM
      REAL, DIMENSION(PKSCLEN), INTENT(INOUT)          :: PKSRL2NORM
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)   :: XC
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)   :: BC
      DOUBLEPRECISION, DIMENSION(NNZC), INTENT(INOUT)   :: AC
      INTEGER, DIMENSION(NIAC+1), INTENT(IN)            :: IAC
      INTEGER, DIMENSION(NNZC), INTENT(IN)              :: JAC
      DOUBLEPRECISION, DIMENSION(NNZAPC), INTENT(INOUT) :: APC
      INTEGER, DIMENSION(NIAPC+1), INTENT(IN)           :: IAPC
      INTEGER, DIMENSION(NJAPC), INTENT(IN)             :: JAPC
C       WORKING ARRAYS
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)   :: DC
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)   :: PC
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)   :: QC
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)   :: ZC
C       DIAGONAL SCALING VECTOR
      INTEGER, INTENT(IN) :: ISCL
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(IN)      :: SCL
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(IN)      :: SCLI
C
      INTEGER, DIMENSION(NIAC), INTENT(IN)             :: IXMAP         !JV
      INTEGER, DIMENSION(*), INTENT(IN)                :: NODEC         !JV
      INTEGER, INTENT(IN)                              :: NODES         !JV
C     + + + LOCAL DEFINITIONS + + +
      DOUBLEPRECISION, PARAMETER :: DZERO = 0.0D0
      DOUBLEPRECISION, PARAMETER :: DONE  = 1.0D0
      DOUBLEPRECISION, PARAMETER :: DPREC = EPSILON(1.0D0)

      INTEGER :: iiter
      INTEGER :: iicnvg
      INTEGER :: I                                                      !JV
 
      REAL :: pct0
      LOGICAL :: LRESUP

      DOUBLEPRECISION :: dhclose, drclose
      DOUBLEPRECISION :: dxmax
      DOUBLEPRECISION :: rmax
      DOUBLEPRECISION :: l2norm
      DOUBLEPRECISION :: rcnvg
      DOUBLEPRECISION :: alpha, beta,
     1                   gamma, ddum                                    !JV
      DOUBLEPRECISION :: rho, rho0
      INTEGER :: lxmax, lrmax
      INTEGER :: PXMAX, PRMAX                                           !JV
      LOGICAL :: WRITESTO                                               !JV
      
C       FUNCTIONS      
      DOUBLEPRECISION :: SPKS_DOTP
      DOUBLEPRECISION :: SPKS_INFNORM
      DOUBLEPRECISION :: SPKS_L2NORM
      INTEGER         :: SPKS_LOCMAX
      LOGICAL         :: PKS7MPIMASTERWRITE                             !JV
      LOGICAL         :: LFIRST                                         !JV
C
C-------CODE
      WRITESTO = PKS7MPIMASTERWRITE()                                   !JV
      LFIRST = .TRUE.                                                   !JV
      DDUM = 0.D0                                                       !JV

C-------INITIALIZE SOLUTION VARIABLE AND ARRAYS
      iiter = 0
      IF ( KITER.EQ.1 ) ITER1 = 0
      ICNVG  = 0
      iicnvg = 0
      alpha  = dzero
      beta   = dzero
      rho    = dzero
      rho0   = dzero
      dhclose = DBLE( hclose )
      drclose = DBLE( rclose )
C
C-------INNER ITERATION          
      INNER: DO iiter = 1, NITER
         IPKSI = IPKSI + 1
         ITER1  = ITER1  + 1
C---------APPLY PRECONDITIONER
        CALL SPKS_TIMER(0,pct0,PKSPCA)
        CALL SPKS_PCA(NCORESV,NPC,NNZC,NIAC,
     2                NIAPC,NJAPC,NNZAPC,
     3                APC,IAC,JAC,IAPC,JAPC,
     4                DC,ZC,
     5                IXMAP,NODES)                                      !JV
        CALL SPKS_TIMER(1,pct0,PKSPCA)
C---------COMPUTE rho
        rho = SPKS_DOTP(NCORESV,NRPROC,NIAC,DC,ZC,IXMAP)                !JV
        CALL PKS7MPIGXCHIP( rho, ddum, 1 )                              !JV
C---------COMPUTE DIRECTIONAL VECTORS
        IF (iiter.GT.1) THEN
          beta = rho / rho0
          CALL SPKS_AXPY(NCORESV,NIAC,ZC,beta,PC,PC)
        ELSE
          CALL SPKS_DCOPY(NCORESV, NIAC, ZC, PC)
        END IF
C---------COMPUTE ITERATES
C         UPDATE qc
        CALL PKS7MPILXCHDUNS(PC, NODEC)                                 !JV
        CALL SPKS_CMATVEC(NCORESM,NIAC,NNZC,IAC,JAC,AC,PC,QC)
C         UPDATE alpha
        gamma = SPKS_DOTP(NCORESV,NRPROC,NIAC,PC,QC,IXMAP)              !JV
        gamma = gamma + SIGN(DPREC, gamma)
        CALL PKS7MPIGXCHIP(gamma, ddum, 1 )                             !JV
        alpha = rho / gamma                                             !JV
C---------UPDATE X AND RESIDUAL
        CALL SPKS_AXPY(NCORESV,NIAC,XC,alpha,PC,XC)
        CALL SPKS_AXPY(NCORESV,NIAC,DC,-alpha,QC,DC)
C---------UNSCALE HEAD CHANGE AND DETERMINE MAXIMUM
        CALL SPKS_DSET(NCORESV,NIAC,DZERO,ZC)
        CALL SPKS_AXPY(NCORESV,NIAC,ZC,alpha,PC,ZC)
        IF ( ISCL.NE.0 ) THEN
          CALL SPKS_VVMULT(NCORESV,NIAC,ZC,SCL,ZC)
        END IF
        dxmax = SPKS_INFNORM(NCORESV,NIAC,ZC,IXMAP)                     !JV
        lxmax = SPKS_LOCMAX(NCORESV,NIAC,ZC,IXMAP)
C---------UNSCALE RESIDUAL AND DETERMINE MAXIMUM
        IF ( ISCL.NE.0 ) THEN
          CALL SPKS_DCOPY(NCORESV, NIAC, DC, ZC)
          CALL SPKS_VVMULT(NCORESV,NIAC,ZC,SCLI,ZC)
          rmax = SPKS_INFNORM(NCORESV,NIAC,ZC,IXMAP)                    !JV
          lrmax = SPKS_LOCMAX(NCORESV,NIAC,ZC,IXMAP)
          l2norm = SPKS_L2NORM(NCORESV,NRPROC,NIAC,ZC,IXMAP)
        ELSE
          rmax = SPKS_INFNORM(NCORESV,NIAC,DC,IXMAP)                    !JV
          lrmax = SPKS_LOCMAX(NCORESV,NIAC,DC,IXMAP)
          l2norm = SPKS_L2NORM(NCORESV,NRPROC,NIAC,DC,IXMAP)
        END IF
C--------DETERMINE STOPPING CRITERIA FOR THE MPI CASE        
        CALL PKS7MPIGXCHCNVG(DXMAX,LXMAX,PXMAX,RMAX,LRMAX,PRMAX,        !JV
     1    RMAX0,L2NORM,NIAC,IXMAP,NODES,LFIRST)                         !JV
        
C---------SAVE CONVERGENCE HISTORY FOR ITERATION
        PKSIT(ITER1) = 0
        IF ( iiter.EQ.1 ) THEN
          PKSIT(ITER1) = 1
        END IF
        PKSHMX(ITER1) = dxmax
        PKSHMXLOC(1,ITER1) = lxmax
        PKSRMX(ITER1) = rmax
        PKSRMXLOC(1,ITER1) = lrmax
        PKSL2NORM(ITER1) = l2norm
        PKSRL2NORM(ITER1) = l2norm * L2NORMBI
C---------TEST FOR SOLVER CONVERGENCE
        IF ( ICNVGOPT.EQ.0 ) THEN
          rcnvg = rmax
        ELSE IF ( ICNVGOPT.EQ.1 ) THEN
          rcnvg = l2norm
        ELSE IF ( ICNVGOPT.EQ.2 ) THEN
          rcnvg = l2norm * L2NORMBI
        END IF
        CALL SPKS_TESTCNVG( ICNVGOPT,KITER,MXITER,iiter,iicnvg,ICNVG,
     &                      RMAX0,EPFACT,dxmax,rcnvg,dhclose,drclose )
         IF (WRITESTO )                                                 !JV
     1    WRITE(*,'(1X,A,I4.4,1X,I4.4,F10.3,E15.5)')                    !JV
     1    'OITER IITER BIGH BIGR = ',                                   !JV
     1    KITER,IITER, dxmax, rmax                                      !JV

        IF ( iicnvg.EQ.1 ) EXIT INNER
C---------RECALCULATE THE RESIDUAL
        IF (NRESUP > 0) THEN
          LRESUP = mod(iiter+1,NRESUP) == 0
          IF (LRESUP) THEN
            CALL SPKS_CMATVEC(NCORESM,NIAC,NNZC,IAC,JAC,AC,XC,DC)
            CALL SPKS_AXPY(NCORESV, NIAC, BC, -DONE, DC, DC)
          END IF
        END IF
C---------SAVE CURRENT INNER ITERATES
        rho0 = rho
      END DO INNER
C
C-------RETURN
      RETURN
C
      END SUBROUTINE SPKS_CG
C
C
      SUBROUTINE SPKS_BICGSTAB(ICNVG,MXITER,KITER,
     &                         NPC,NCORESM,NCORESV,
     &                         NRPROC,                                  !JV
     &                         NITER,ITER1,NNZC,NIAC,
     &                         NIAPC,NJAPC,NNZAPC,
     &                         ICNVGOPT,RMAX0,L2NORMBI,EPFACT,
     &                         HCLOSE,RCLOSE,NRESUP,
     &                         IPKSI,PKSPCA,
     &                         PKSCLEN,PKSIT,PKSHMXLOC,PKSHMX,
     &                         PKSRMXLOC,PKSRMX,PKSL2NORM,PKSRL2NORM,
     &                         XC,BC,AC,IAC,JAC,
     &                         APC,IAPC,JAPC,
     &                         DC,ZC,PC,QC,VC,
     &                         DTILC,PHATC,DHATC,
     &                         ISCL,SCL,SCLI,
     &                         IXMAP,NODEC,NODES)                       !JV
C
      IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
      INTEGER, INTENT(INOUT)                           :: ICNVG
      INTEGER, INTENT(IN)                              :: MXITER
      INTEGER, INTENT(IN)                              :: KITER
      INTEGER, INTENT(IN)                              :: NPC
      INTEGER, INTENT(IN)                              :: NCORESM
      INTEGER, INTENT(IN)                              :: NCORESV
      INTEGER, INTENT(IN)                              :: NRPROC        !JV
      INTEGER, INTENT(IN)                               :: NITER
      INTEGER, INTENT(INOUT)                            :: ITER1
      INTEGER, INTENT(IN)                               :: NNZC
      INTEGER, INTENT(IN)                               :: NIAC
      INTEGER, INTENT(IN)                               :: NIAPC
      INTEGER, INTENT(IN)                               :: NJAPC
      INTEGER, INTENT(IN)                               :: NNZAPC
      INTEGER, INTENT(IN)                               :: ICNVGOPT
      DOUBLEPRECISION, INTENT(INOUT)                    :: RMAX0
      DOUBLEPRECISION, INTENT(IN)                       :: L2NORMBI
      DOUBLEPRECISION, INTENT(IN)                       :: EPFACT
      REAL, INTENT(IN)                                  :: HCLOSE
      REAL, INTENT(IN)                                  :: RCLOSE
      INTEGER, INTENT(IN)                               :: NRESUP
      INTEGER, INTENT(INOUT)                            :: IPKSI
      REAL, INTENT(INOUT)                               :: PKSPCA
      INTEGER, INTENT(IN)                               :: PKSCLEN
      INTEGER, DIMENSION(PKSCLEN), INTENT(INOUT)       :: PKSIT
      INTEGER, DIMENSION(3,PKSCLEN), INTENT(INOUT)     :: PKSHMXLOC
      REAL, DIMENSION(PKSCLEN), INTENT(INOUT)          :: PKSHMX
      INTEGER, DIMENSION(3,PKSCLEN), INTENT(INOUT)     :: PKSRMXLOC
      REAL, DIMENSION(PKSCLEN), INTENT(INOUT)          :: PKSRMX
      REAL, DIMENSION(PKSCLEN), INTENT(INOUT)          :: PKSL2NORM
      REAL, DIMENSION(PKSCLEN), INTENT(INOUT)          :: PKSRL2NORM
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)   :: XC
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)   :: BC
      DOUBLEPRECISION, DIMENSION(NNZC), INTENT(INOUT)   :: AC
      INTEGER, DIMENSION(NIAC+1), INTENT(IN)            :: IAC
      INTEGER, DIMENSION(NNZC), INTENT(IN)              :: JAC
      DOUBLEPRECISION, DIMENSION(NNZAPC), INTENT(INOUT) :: APC
      INTEGER, DIMENSION(NIAPC+1), INTENT(IN)           :: IAPC
      INTEGER, DIMENSION(NJAPC), INTENT(IN)             :: JAPC
C       WORKING ARRAYS
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)   :: DC
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)   :: PC
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)   :: QC
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)   :: ZC
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)   :: VC
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)   :: DTILC
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)   :: PHATC
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)   :: DHATC
C       DIAGONAL SCALING VECTOR
      INTEGER, INTENT(IN) :: ISCL
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(IN)      :: SCL
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(IN)      :: SCLI
C
      INTEGER, DIMENSION(NIAC), INTENT(IN)             :: IXMAP         !JV
      INTEGER, DIMENSION(*), INTENT(IN)                :: NODEC         !JV
      INTEGER, INTENT(IN)                              :: NODES         !JV
C     + + + LOCAL DEFINITIONS + + +
      DOUBLEPRECISION, PARAMETER :: DZERO = 0.0D0
      DOUBLEPRECISION, PARAMETER :: DONE  = 1.0D0
      DOUBLEPRECISION, PARAMETER :: DPREC = EPSILON(1.0D0)
      INTEGER :: iiter
      INTEGER :: iicnvg
      
      REAL :: pct0
      LOGICAL :: LRESUP


      DOUBLEPRECISION :: dhclose, drclose
      DOUBLEPRECISION :: dxmax
      DOUBLEPRECISION :: rmax
      DOUBLEPRECISION :: l2norm
      DOUBLEPRECISION :: rcnvg
      DOUBLEPRECISION :: numer, denom
      DOUBLEPRECISION :: alpha
      DOUBLEPRECISION :: beta
      DOUBLEPRECISION :: rho, rho0
      DOUBLEPRECISION :: omega
      INTEGER :: lxmax, lrmax
      INTEGER :: PXMAX, PRMAX                                           !JV
      LOGICAL         :: WRITESTO                                       !JV
      DOUBLEPRECISION :: machprec, ddum                                 !JV
C       FUNCTIONS      
      DOUBLEPRECISION :: SPKS_DOTP
      DOUBLEPRECISION :: SPKS_INFNORM
      INTEGER         :: SPKS_LOCMAX
      DOUBLEPRECISION :: SPKS_L2NORM
      LOGICAL         :: PKS7MPIMASTERWRITE                             !JV
      LOGICAL         :: LFIRST                                         !JV
C
C-------CODE
      WRITESTO = PKS7MPIMASTERWRITE()                                   !JV
      LFIRST = .TRUE.                                                   !JV
      DDUM = 0.D0                                                       !JV
C
C-------INITIALIZE SOLUTION VARIABLE AND ARRAYS
      iiter = 0
      IF ( KITER.EQ.1 ) ITER1 = 0
      ICNVG  = 0
      iicnvg = 0
      alpha  = dzero
      beta   = dzero
      rho    = dzero
      rho0   = dzero
      dhclose = DBLE( hclose )
      drclose = DBLE( rclose )
      machprec = EPSILON( DZERO )                                       !JV
C
C-------SAVE INITIAL RESIDUAL
      CALL SPKS_DCOPY(NCORESV, NIAC, DC, DTILC)
C
C-------INNER ITERATION          
      INNER: DO iiter = 1, NITER
         IPKSI = IPKSI + 1
         ITER1  = ITER1  + 1
C---------CALCULATE rho
        rho = SPKS_DOTP(NCORESV,NRPROC,NIAC,DTILC,DC,IXMAP)             !JV
        CALL PKS7MPIGXCHIP( rho, ddum, 1 )                              !JV
C---------COMPUTE DIRECTIONAL VECTORS
        beta = (rho / rho0) * (alpha / omega)
        CALL SPKS_AXPY(NCORESV,NIAC,PC,-omega,VC,PC)                    !JV 
        CALL SPKS_AXPY(NCORESV,NIAC,DC,beta,PC,PC)
C---------APPLY PRECONDITIONER
        CALL SPKS_TIMER(0,pct0,PKSPCA)
        CALL SPKS_PCA(NCORESV,NPC,NNZC,NIAC,
     2                NIAPC,NJAPC,NNZAPC,
     3                APC,IAC,JAC,IAPC,JAPC,
     4                PC,PHATC,
     5                IXMAP,NODES)                                      !JV
        CALL SPKS_TIMER(1,pct0,PKSPCA)
C---------UPDATE V WITH A AND PHATC
        CALL PKS7MPILXCHDUNS(PHATC, NODEC)                              !JV
        CALL SPKS_CMATVEC(NCORESM,NIAC,NNZC,IAC,JAC,AC,PHATC,VC)
C---------UPDATE alpha
        denom = SPKS_DOTP(NCORESV,NRPROC,NIAC,DTILC,VC,IXMAP)           !JV
        CALL PKS7MPIGXCHIP( denom, ddum, 1 )                            !JV
        alpha = rho / denom
C---------UPDATE RESIDUAL (DC) USING alpha and VC
        CALL SPKS_AXPY(NCORESV,NIAC,DC,-alpha,VC,DC)
C---------CALCULATE INFINITY-NORM OF DC - USED TO TEST FOR TERMINATION
        IF ( ISCL.NE.0 ) THEN
          CALL SPKS_DCOPY(NCORESV, NIAC, DC, ZC)
          CALL SPKS_VVMULT(NCORESV,NIAC,ZC,SCLI,ZC)
          rmax = SPKS_INFNORM(NCORESV,NIAC,ZC,IXMAP)                    !JV
          lrmax = SPKS_LOCMAX(NCORESV,NIAC,ZC,IXMAP)
          l2norm = SPKS_L2NORM(NCORESV,NRPROC,NIAC,ZC,IXMAP)
        ELSE
          rmax = SPKS_INFNORM(NCORESV,NIAC,DC,IXMAP)                    !JV
          lrmax = SPKS_LOCMAX(NCORESV,NIAC,DC,IXMAP)
          l2norm = SPKS_L2NORM(NCORESV,NRPROC,NIAC,DC,IXMAP)
        END IF
C---------GET MAXIMUM RESIDUAL FOR ALL IMAGES
        CALL PKS7MPIGXCHMAXD1( rmax )                                   !JV
        IF ( abs(rmax).LE.machprec ) THEN                               !JV
C-----------CALCULATE DX
          CALL SPKS_DSET(NCORESV,NIAC,DZERO,ZC)
          CALL SPKS_AXPY(NCORESV,NIAC,ZC,alpha,PHATC,ZC)
C-----------GET MAXIMUM UNSCALED HEAD CHANGE          
          IF ( ISCL.NE.0 ) THEN
            CALL SPKS_VVMULT(NCORESV,NIAC,ZC,SCL,ZC)
          END IF
          dxmax = SPKS_INFNORM(NCORESV,NIAC,ZC,IXMAP)                   !JV
          lxmax = SPKS_LOCMAX(NCORESV,NIAC,ZC,IXMAP)
C-----------UPDATE X          
          CALL SPKS_AXPY(NCORESV,NIAC,XC,alpha,PHATC,XC)
C-----------DETERMINE STOPPING CRITERIA FOR THE MPI CASE        
          CALL PKS7MPIGXCHCNVG(DXMAX,LXMAX,PXMAX,RMAX,LRMAX,PRMAX,      !JV
     1       RMAX0,L2NORM,NIAC,IXMAP,NODES,LFIRST)                      !JV
C-----------TEST FOR SOLVER CONVERGENCE
          IF ( ICNVGOPT.EQ.0 ) THEN
            rcnvg = rmax
          ELSE IF ( ICNVGOPT.EQ.1 ) THEN
            rcnvg = l2norm
          ELSE IF ( ICNVGOPT.EQ.2 ) THEN
            rcnvg = l2norm * L2NORMBI
          END IF
          CALL SPKS_TESTCNVG( ICNVGOPT,KITER,MXITER,iiter,iicnvg,ICNVG,
     &                        RMAX0,EPFACT,dxmax,rcnvg,dhclose,drclose )
                 
          IF (WRITESTO )                                                !JV
     1      WRITE(*,'(1X,A,I4.4,1X,I4.4,F10.3,E15.5)')                  !JV
     1      '1: OITER IITER BIGH BIGR = ',                              !JV
     1      KITER,IITER, dxmax, rmax                                    !JV
          
          IF ( iicnvg.EQ.1 ) THEN
C             SAVE CONVERGENCE HISTORY FOR ITERATION
            PKSIT(ITER1) = 0
            IF ( iiter.EQ.1 ) THEN
              PKSIT(ITER1) = 1
            END IF
            PKSHMX(ITER1) = dxmax
            PKSHMXLOC(1,ITER1) = lxmax
            PKSRMX(ITER1) = rmax
            PKSRMXLOC(1,ITER1) = lrmax
            PKSL2NORM(ITER1) = l2norm
            PKSRL2NORM(ITER1) = l2norm * L2NORMBI
C
            EXIT INNER
          END IF
        END IF
C---------APPLY PRECONDITIONER TO UPDATE DHATC
        CALL SPKS_TIMER(0,pct0,PKSPCA)
        CALL SPKS_PCA(NCORESV,NPC,NNZC,NIAC,
     2                NIAPC,NJAPC,NNZAPC,
     3                APC,IAC,JAC,IAPC,JAPC,
     4                DC,DHATC,
     5                IXMAP,NODES)                                      !JV
        CALL SPKS_TIMER(1,pct0,PKSPCA)
C---------UPDATE T WITH A AND DHATC
        CALL PKS7MPILXCHDUNS(DHATC, NODEC)                              !JV
        CALL SPKS_CMATVEC(NCORESM,NIAC,NNZC,IAC,JAC,AC,DHATC,QC)
C---------UPDATE omega
        numer = SPKS_DOTP(NCORESV,NRPROC,NIAC,QC,DC,IXMAP)              !JV
        denom = SPKS_DOTP(NCORESV,NRPROC,NIAC,QC,QC,IXMAP)              !JV
        CALL PKS7MPIGXCHIP( numer, denom, 2 )                           !JV
        omega = numer / denom
C---------CALCULATE DX AND PUT IN ZC
        CALL SPKS_DSET(NCORESV,NIAC,DZERO,ZC)
        CALL SPKS_AXPY(NCORESV,NIAC,ZC,alpha,PHATC,ZC)
        CALL SPKS_AXPY(NCORESV,NIAC,ZC,omega,DHATC,ZC)
C---------UPDATE X AND RESIDUAL
        CALL SPKS_AXPY(NCORESV,NIAC,XC,done,ZC,XC)
        CALL SPKS_AXPY(NCORESV,NIAC,DC,-omega,QC,DC)
C---------UNSCALE HEAD CHANGE AND DETERMINE MAXIMUM
        IF ( ISCL.NE.0 ) THEN
          CALL SPKS_VVMULT(NCORESV,NIAC,ZC,SCL,ZC)
        END IF
        dxmax = SPKS_INFNORM(NCORESV,NIAC,ZC,IXMAP)                     !JV
        lxmax = SPKS_LOCMAX(NCORESV,NIAC,ZC,IXMAP)
C---------UNSCALE RESIDUAL AND DETERMINE MAXIMUM
        IF ( ISCL.NE.0 ) THEN
          CALL SPKS_DCOPY(NCORESV, NIAC, DC, ZC)
          CALL SPKS_VVMULT(NCORESV,NIAC,ZC,SCLI,ZC)
          rmax = SPKS_INFNORM(NCORESV,NIAC,ZC,IXMAP)                    !JV
          lrmax = SPKS_LOCMAX(NCORESV,NIAC,ZC,IXMAP)
          l2norm = SPKS_L2NORM(NCORESV,NRPROC,NIAC,ZC,IXMAP)
        ELSE
          rmax = SPKS_INFNORM(NCORESV,NIAC,DC,IXMAP)                    !JV
          lrmax = SPKS_LOCMAX(NCORESV,NIAC,DC,IXMAP)
          l2norm = SPKS_L2NORM(NCORESV,NRPROC,NIAC,DC,IXMAP)
        END IF
C---------DETERMINE MAXIMUM HEAD CHANGE AND RESIDUAL
C-----------DETERMINE STOPPING CRITERIA FOR THE MPI CASE        
        CALL PKS7MPIGXCHCNVG(DXMAX,LXMAX,PXMAX,RMAX,LRMAX,PRMAX,        !JV
     1       RMAX0,L2NORM,NIAC,IXMAP,NODES,LFIRST)                      !JV
C---------SAVE CONVERGENCE HISTORY FOR ITERATION
        PKSIT(ITER1) = 0
        IF ( iiter.EQ.1 ) THEN
          PKSIT(ITER1) = 1
        END IF
        PKSHMX(ITER1) = dxmax
        PKSHMXLOC(1,ITER1) = lxmax
        PKSRMX(ITER1) = rmax
        PKSRMXLOC(1,ITER1) = lrmax
        PKSL2NORM(ITER1) = l2norm
        PKSRL2NORM(ITER1) = l2norm * L2NORMBI
C---------TEST FOR SOLVER CONVERGENCE
        IF ( ICNVGOPT.EQ.0 ) THEN
          rcnvg = rmax
        ELSE IF ( ICNVGOPT.EQ.1 ) THEN
          rcnvg = l2norm
        ELSE IF ( ICNVGOPT.EQ.2 ) THEN
          rcnvg = l2norm * L2NORMBI
        END IF
        CALL SPKS_TESTCNVG( ICNVGOPT,KITER,MXITER,iiter,iicnvg,ICNVG,
     &                      RMAX0,EPFACT,dxmax,rcnvg,dhclose,drclose )
C
        IF (WRITESTO )                                                  !JV
     1    WRITE(*,'(1X,A,I4.4,1X,I4.4,F10.3,E15.5)')                    !JV
     1    '2: OITER IITER BIGH BIGR = ',                                !JV
     1    KITER,IITER, dxmax, rmax                                      !JV
C
        IF ( iicnvg.EQ.1 ) THEN
          EXIT INNER
        END IF
C---------RECALCULATE THE RESIDUAL
        IF (NRESUP > 0) THEN
          LRESUP = mod(iiter+1,NRESUP) == 0
          IF (LRESUP) THEN
            CALL SPKS_CMATVEC(NCORESM,NIAC,NNZC,IAC,JAC,AC,XC,DC)
            CALL SPKS_AXPY(NCORESV, NIAC, BC, -DONE, DC, DC)
          END IF
        END IF
C---------SAVE CURRENT INNER ITERATE
        rho0 = rho
      END DO INNER
C
C-------RETURN
      RETURN
C
      END SUBROUTINE SPKS_BICGSTAB
C
      SUBROUTINE SPKS_GMRES(ICNVG,MXITER,KITER,
     &                         NPC,NCORESM,NCORESV,
     &                         NRPROC,                                  !JV
     &                         NITER,ITER1,NNZC,NIAC,NREST,
     &                         NIAPC,NJAPC,NNZAPC,
     &                         ICNVGOPT,RMAX0,L2NORMBI,EPFACT,
     &                         HCLOSE,RCLOSE,NRESUP,
     &                         IPKSI,PKSPCA,
     &                         PKSCLEN,PKSIT,PKSHMXLOC,PKSHMX,
     &                         PKSRMXLOC,PKSRMX,PKSL2NORM,PKSRL2NORM,
     &                         XC,BC,AC,IAC,JAC,
     &                         APC,IAPC,JAPC,
     &                         DC,ZC,PC,QC,
     &                         CSG,SNG,SG,YG,HG,VG,
     &                         ISCL,SCL,SCLI,
     &                         IXMAP,NODEC,NODES)                       !JV
C
      IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
      INTEGER, INTENT(INOUT)                            :: ICNVG
      INTEGER, INTENT(IN)                               :: MXITER
      INTEGER, INTENT(IN)                               :: KITER
      INTEGER, INTENT(IN)                               :: NPC
      INTEGER, INTENT(IN)                               :: NCORESM
      INTEGER, INTENT(IN)                               :: NCORESV
      INTEGER, INTENT(IN)                               :: NRPROC        !JV
      INTEGER, INTENT(IN)                               :: NITER
      INTEGER, INTENT(INOUT)                            :: ITER1
      INTEGER, INTENT(IN)                               :: NNZC
      INTEGER, INTENT(IN)                               :: NIAC
      INTEGER, INTENT(IN)                               :: NREST
      INTEGER, INTENT(IN)                               :: NIAPC
      INTEGER, INTENT(IN)                               :: NJAPC
      INTEGER, INTENT(IN)                               :: NNZAPC
      INTEGER, INTENT(IN)                               :: ICNVGOPT
      DOUBLEPRECISION, INTENT(INOUT)                    :: RMAX0
      DOUBLEPRECISION, INTENT(IN)                       :: L2NORMBI
      DOUBLEPRECISION, INTENT(IN)                       :: EPFACT
      REAL, INTENT(IN)                                  :: HCLOSE
      REAL, INTENT(IN)                                  :: RCLOSE
      INTEGER, INTENT(IN)                               :: NRESUP
      INTEGER, INTENT(INOUT)                            :: IPKSI
      REAL, INTENT(INOUT)                               :: PKSPCA
      INTEGER, INTENT(IN)                               :: PKSCLEN
      INTEGER, DIMENSION(PKSCLEN), INTENT(INOUT)        :: PKSIT
      INTEGER, DIMENSION(3,PKSCLEN), INTENT(INOUT)      :: PKSHMXLOC
      REAL, DIMENSION(PKSCLEN), INTENT(INOUT)           :: PKSHMX
      INTEGER, DIMENSION(3,PKSCLEN), INTENT(INOUT)      :: PKSRMXLOC
      REAL, DIMENSION(PKSCLEN), INTENT(INOUT)           :: PKSRMX
      REAL, DIMENSION(PKSCLEN), INTENT(INOUT)           :: PKSL2NORM
      REAL, DIMENSION(PKSCLEN), INTENT(INOUT)           :: PKSRL2NORM
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)   :: XC
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)   :: BC
      DOUBLEPRECISION, DIMENSION(NNZC), INTENT(INOUT)   :: AC
      INTEGER, DIMENSION(NIAC+1), INTENT(IN)            :: IAC
      INTEGER, DIMENSION(NNZC), INTENT(IN)              :: JAC
      DOUBLEPRECISION, DIMENSION(NNZAPC), INTENT(INOUT) :: APC
      INTEGER, DIMENSION(NIAPC+1), INTENT(IN)           :: IAPC
      INTEGER, DIMENSION(NJAPC), INTENT(IN)             :: JAPC
C       WORKING ARRAYS
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)          :: DC
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)          :: PC
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)          :: QC
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)          :: ZC
      DOUBLEPRECISION, DIMENSION(NREST), INTENT(INOUT)         :: CSG
      DOUBLEPRECISION, DIMENSION(NREST), INTENT(INOUT)         :: SNG
      DOUBLEPRECISION, DIMENSION(NREST+1), INTENT(INOUT)       :: SG
      DOUBLEPRECISION, DIMENSION(NREST+1), INTENT(INOUT)       :: YG
      DOUBLEPRECISION, DIMENSION(NREST+1,NREST), INTENT(INOUT) :: HG
      DOUBLEPRECISION, DIMENSION(NIAC,NREST+1), INTENT(INOUT)  :: VG
C       DIAGONAL SCALING VECTOR
      INTEGER, INTENT(IN) :: ISCL
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(IN)      :: SCL
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(IN)      :: SCLI
C
      INTEGER, DIMENSION(NIAC), INTENT(IN)             :: IXMAP         !JV
      INTEGER, DIMENSION(*), INTENT(IN)                :: NODEC         !JV
      INTEGER, INTENT(IN)                              :: NODES         !JV
C     + + + LOCAL DEFINITIONS + + +
      DOUBLEPRECISION, PARAMETER :: DZERO = 0.0D0
      DOUBLEPRECISION, PARAMETER :: DONE  = 1.0D0
      DOUBLEPRECISION, PARAMETER :: DELTA = 1.0D-3
      DOUBLEPRECISION, PARAMETER :: DPREC = EPSILON(1.0D0)
      INTEGER :: iiter
      INTEGER :: iicnvg
      
      INTEGER :: n
      INTEGER :: irst
      INTEGER :: igmr
      INTEGER :: i, ii
      INTEGER :: jj
      INTEGER :: ilen
      
      DOUBLEPRECISION :: ht
      DOUBLEPRECISION :: mu
      DOUBLEPRECISION :: dn
      DOUBLEPRECISION :: deltax
      DOUBLEPRECISION :: dx
      DOUBLEPRECISION :: usdx
      
      REAL :: pct0
      
      LOGICAL :: LRESUP

      DOUBLEPRECISION :: dhclose, drclose
      DOUBLEPRECISION :: dxmax
      DOUBLEPRECISION :: rmax
      DOUBLEPRECISION :: l2norm
      DOUBLEPRECISION :: rcnvg
      DOUBLEPRECISION :: numer, denom
      DOUBLEPRECISION :: alpha
      DOUBLEPRECISION :: beta, betai
      DOUBLEPRECISION :: rho, rho0
      DOUBLEPRECISION :: omega
      INTEGER :: lxmax, lrmax
      INTEGER :: PXMAX, PRMAX                                           !JV
      LOGICAL         :: WRITESTO                                       !JV
C       FUNCTIONS      
      DOUBLEPRECISION :: SPKS_DOTP
      DOUBLEPRECISION :: SPKS_INFNORM
      INTEGER         :: SPKS_LOCMAX
      DOUBLEPRECISION :: SPKS_L2NORM
      LOGICAL         :: PKS7MPIMASTERWRITE                             !JV
      LOGICAL         :: LFIRST                                         !JV
C
C-------CODE
      WRITESTO = PKS7MPIMASTERWRITE()                                   !JV
      LFIRST = .TRUE.                                                   !JV
C
C-------INITIALIZE SOLUTION VARIABLE AND ARRAYS
      iiter = 0
      IF ( KITER.EQ.1 ) ITER1 = 0
      ICNVG  = 0
      iicnvg = 0
      alpha  = dzero
      beta   = dzero
      rho    = dzero
      rho0   = done
      omega  = done
      dhclose = DBLE( hclose )
      drclose = DBLE( rclose )
C
C---------RESTART LOOP FOR LINEAR GMRES
        irst = 1
        GMRST: DO          
          IPKSI = IPKSI + 1
          ITER1 = ITER1  + 1
          iiter = iiter + 1
C-----------APPLY PRECONDITIONER
          CALL SPKS_TIMER(0,pct0,PKSPCA)
          CALL SPKS_PCA(NCORESV,NPC,NNZC,NIAC,
     2                  NIAPC,NJAPC,NNZAPC,
     3                  APC,IAC,JAC,IAPC,JAPC,
     4                  DC,ZC,
     5                  IXMAP,NODES)                                    !JV
          CALL SPKS_TIMER(1,pct0,PKSPCA)  
C-----------RESET GMRES VARIABLES PRIOR TO EACH RESTART ITERATION
C-----------PUSH ZC INTO PC          
          CALL SPKS_DCOPY(NCORESV, NIAC, ZC, PC)
C-----------RESET OTHER VARIABLES
          CALL SPKS_DSET(NCORESV,NREST,DZERO,CSG)
          CALL SPKS_DSET(NCORESV,NREST,DZERO,SNG)
          CALL SPKS_DSET(NCORESV,NREST+1,DZERO,SG)
          CALL SPKS_DSET(NCORESV,(NREST+1)*NREST,DZERO,HG)
          CALL SPKS_DSET(NCORESV,NIAC*(NREST+1),DZERO,VG)
          beta = SPKS_L2NORM(NCORESV,NRPROC,NIAC,ZC,IXMAP)
          SG(1) = beta
          DO n = 1, NIAC
            VG(n,1) = PC(n) / beta
          END DO
          !VG(1:NIAC,1) = PC / beta

          GMITER: DO igmr = 1, NREST
            ii = igmr
            jj = ii
C-------------UPDATE QC WITH A AND VG(:,igmr)            
            CALL PKS7MPILXCHDUNS(VG(1:NIAC,igmr), NODEC)                !JV
            CALL SPKS_CMATVEC(NCORESM,NIAC,NNZC,IAC,JAC,AC,
     &                        VG(1:NIAC,igmr),QC)
C-------------APPLY PRECONDITIONER TO UPDATE VG(:,igmr+1)
            CALL SPKS_TIMER(0,pct0,PKSPCA)
            CALL SPKS_PCA(NCORESV,NPC,NNZC,NIAC,
     2                    NIAPC,NJAPC,NNZAPC,
     3                    APC,IAC,JAC,IAPC,JAPC,
     4                    QC,VG(1:NIAC,igmr+1),
     5                    IXMAP,NODES)                                  !JV
            CALL SPKS_TIMER(1,pct0,PKSPCA)
C-------------CALCULATE alpha
            alpha = SPKS_L2NORM(NCORESV,NRPROC,NIAC,
     &                          VG(1:NIAC,igmr+1),IXMAP)
            DO i = 1, igmr
              rho = SPKS_DOTP(NCORESV,NRPROC,NIAC,
     &                        VG(1:NIAC,igmr+1),
     &                        VG(1:NIAC,i),IXMAP) 
              HG(i,igmr) = rho
              CALL SPKS_AXPY(NCORESV,NIAC,VG(1:NIAC,igmr+1),
     &                       -rho,VG(1:NIAC,i),VG(1:NIAC,igmr+1))
            END DO
            HG(igmr+1,igmr) = SPKS_L2NORM(NCORESV,NRPROC,NIAC,
     &                                    VG(1:NIAC,igmr+1),IXMAP)
            
            !*** global communication: alpha, hg ??? ***
            
            IF ( (alpha + DELTA * HG(igmr+1,igmr) ).EQ.alpha ) THEN
              DO i = 1, igmr
                ht = SPKS_DOTP(NCORESV,NRPROC,NIAC,
     &                        VG(1:NIAC,igmr+1),
     &                        VG(1:NIAC,i),IXMAP) 
                HG(i,igmr)   = HG(i,igmr) + ht
                CALL SPKS_AXPY(NCORESV,NIAC,VG(1:NIAC,igmr+1),
     &                       -ht,VG(1:NIAC,i),VG(1:NIAC,igmr+1))
              END DO
              HG(igmr+1,igmr) = SPKS_L2NORM(NCORESV,NRPROC,NIAC,
     &                                      VG(1:NIAC,igmr+1),IXMAP)
              
            !*** global communication: ht ??? ***
              
            END IF            
            
            IF ( HG(igmr+1,igmr).NE.DZERO ) THEN
              VG(:,igmr+1) = VG(:,igmr+1) / HG(igmr+1,igmr)
            END IF
            IF ( igmr.GT.1 ) THEN
              CALL SPKS_DCOPY(NCORESV, igmr+1, HG(1:igmr+1,igmr), YG)
              DO i = 1, igmr-1
                CALL SPKS_MGVNS( i, CSG(i), SNG(i), YG(1:igmr+1) )
              END DO
              CALL SPKS_DCOPY(NCORESV, igmr+1, YG, HG(1:igmr+1,igmr))
            END IF
            mu             = SQRT(HG(igmr,igmr)**2 + HG(igmr+1,igmr)**2)
            CSG(igmr)      = HG(igmr,igmr) / mu
            SNG(igmr)      = -HG(igmr+1,igmr) / mu
            HG(igmr,igmr)  = CSG(igmr) * HG(igmr,igmr) - 
     2                       SNG(igmr) * HG(igmr+1,igmr)
            HG(igmr+1,igmr) = DZERO
            CALL SPKS_MGVNS( igmr, CSG(igmr), SNG(igmr), SG(1:igmr+1) )
            l2norm  = ABS( SG(igmr+1) )
C-------------TEST FOR OVERSOLVING
            IF ( l2norm.LT.drclose .OR. igmr.EQ.NREST ) THEN
              ii = ii - 1
              YG(ii+1) = SG(ii+1) / HG(ii+1,ii+1)
              DO i = ii, 1, -1
                ilen = (ii+1) - (i+1) + 1
                ht = SPKS_DOTP(NCORESV,NRPROC,ilen,
     &                        HG(i,i+1:ii+1),
     &                        YG(i+1:ii+1),IXMAP) 
                
                !*** global sum ht ***
                
                !CALL PKS7MPIGXCHIP( ht, 0., 1 )  
                YG(i) = (SG(i) - ht) / HG(i,i) 
              END DO
C---------------UPDATE X

              !*** dx --> array and local exchange *** 

              dxmax = DZERO
              DO n = 1, NIAC
                dx = SPKS_DOTP(NCORESV,NRPROC,(ii+1),
     &                         VG(n,1:ii+1),
     &                         YG(1:ii+1),IXMAP) 
                XC(n) = XC(n) + dx
                IF ( ISCL.NE.0 ) THEN
                  usdx   = ABS( dx * SCLI(n) )
                ELSE
                  usdx   = ABS( dx )
                END IF
                IF ( abs(usdx).GT.abs(dxmax) ) THEN
                  dxmax = abs(usdx)
                  lxmax = n
                END IF
              END DO
              EXIT GMITER
            END IF
          END DO GMITER
          
          !*** excnhange xc, dxmax, lxmax
          
C-----------UPDATE RESIDUAL
          CALL SPKS_CMATVEC(NCORESM,NIAC,NNZC,IAC,JAC,AC,XC,QC)
          CALL SPKS_AXPY(NCORESV, NIAC, BC, -DONE, QC, DC)
C-----------UNSCALE RESIDUAL AND DETERMINE MAXIMUM RESIDUAL
          IF ( ISCL.NE.0 ) THEN
            CALL SPKS_DCOPY(NCORESV, NIAC, DC, ZC)
            CALL SPKS_VVMULT(NCORESV,NIAC,ZC,SCLI,ZC)
            rmax = SPKS_INFNORM(NCORESV,NIAC,ZC,IXMAP)                  !JV
            lrmax = SPKS_LOCMAX(NCORESV,NIAC,ZC,IXMAP)
            l2norm = SPKS_L2NORM(NCORESV,NRPROC,NIAC,ZC,IXMAP)
          ELSE
            rmax = SPKS_INFNORM(NCORESV,NIAC,DC,IXMAP)                  !JV
            lrmax = SPKS_LOCMAX(NCORESV,NIAC,DC,IXMAP)
            l2norm = SPKS_L2NORM(NCORESV,NRPROC,NIAC,DC,IXMAP)
          END IF
C-----------DETERMINE MAXIMUM HEAD CHANGE AND RESIDUAL
          !CALL PKS7MPIGXCHMAXD2( DXMAX, RMAX )                            !JV
C-----------SAVE CONVERGENCE HISTORY FOR ITERATION
          PKSIT(ITER1) = 0
          IF ( iiter.EQ.1 ) THEN
            PKSIT(ITER1) = 1
          END IF
          PKSHMX(ITER1) = dxmax
          PKSHMXLOC(1,ITER1) = lxmax
          PKSRMX(ITER1) = rmax
          PKSRMXLOC(1,ITER1) = lrmax
          PKSL2NORM(ITER1) = l2norm
          PKSRL2NORM(ITER1) = l2norm * L2NORMBI
C-----------TEST FOR SOLVER CONVERGENCE
          IF ( ICNVGOPT.EQ.0 ) THEN
            rcnvg = rmax
          ELSE IF ( ICNVGOPT.EQ.1 ) THEN
            rcnvg = l2norm
          ELSE IF ( ICNVGOPT.EQ.2 ) THEN
            rcnvg = l2norm * L2NORMBI
          END IF
          CALL SPKS_TESTCNVG( ICNVGOPT,KITER,MXITER,iiter,iicnvg,ICNVG,
     &                        RMAX0,EPFACT,dxmax,rcnvg,dhclose,drclose )

          IF (WRITESTO )                                                !JV
     1      WRITE(*,'(1X,A,I4.4,1X,I4.4,F10.3,E15.5)')                  !JV
     1      'OITER IITER BIGH BIGR = ',                                 !JV
     1      KITER,IITER, dxmax, rmax                                    !JV
!C
!C-----------WRITE SOME INFO TO THE SCREEN
!          WRITE(*,'(1X,A,4(I4,1X),G10.3,G10.3)')
!     &      'OITER IITER IREST II BIGH BIGR = ',
!     &      KITER, IITER, irst, jj, dxmax, l2norm
C-----------TERMINATE GMRES IF CONVERGENCE OF NUMBER OF RESTARTS 
C           ARE GREATER THAN NINNER
          IF ( iicnvg.EQ.1 .OR. irst.EQ.NITER ) THEN
            EXIT GMRST
          END IF
C-----------TERMINATE RESTARTS IF NUMBER OF RESTARTS GREATER THAN NINNER
          irst = irst + 1
        END DO GMRST
C
C-------RETURN
      RETURN
C
      END SUBROUTINE SPKS_GMRES
      
      SUBROUTINE SPKS_MGVNS(N, Cs, Sn, G)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN)                            :: N
        DOUBLEPRECISION, INTENT(IN)                    :: Cs
        DOUBLEPRECISION, INTENT(IN)                    :: Sn
        DOUBLEPRECISION, DIMENSION(N+1), INTENT(INOUT) :: G
C     + + + LOCAL DEFINITIONS + + +
        DOUBLEPRECISION :: g1, g2
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        g1     = Cs * G(N) - Sn * G(N+1)
        g2     = Sn * G(N) + Cs * G(N+1)
        G(N)   = g1
        G(N+1) = g2
C---------RETURN
        RETURN
      END SUBROUTINE SPKS_MGVNS
C
      SUBROUTINE PKS7DAUNS() 
      IMPLICIT NONE
      INTEGER, PARAMETER :: IGRID = 1
      CALL PKS7DA(IGRID)
      RETURN
      END SUBROUTINE PKS7DAUNS
C
      SUBROUTINE PKS7DA(IGRID)
C  Deallocate PKS DATA
        USE PKSMODULE
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: IGRID
C     + + + LOCAL DEFINITIONS + + +
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
C
C         SET POINTERS
        CALL PKS7PNT(IGRID)
C
C         DEALLOCATE PKS MEMORY
#ifdef PKSUNS
        DEALLOCATE(CRDUM,CCDUM,CVDUM,HCOFDUM,RHSDUM)
#endif        
        DEALLOCATE(ISOLVER)
        DEALLOCATE(INNERIT,NPC,NOPT,NCORESM,NCORESV)
        DEALLOCATE(ICNVGOPT)
        DEALLOCATE(IEPFACT)
        DEALLOCATE(NITERC,NNZC,NIAC,NIABCGS)
        DEALLOCATE(NGMRES,NREST)
        DEALLOCATE(NIAPC,NJAPC,NNZAPC)
        DEALLOCATE(NIAPCM,NJAPCM)
        DEALLOCATE(NIWC,NWC)
        DEALLOCATE(HCLOSEPKS,RCLOSEPKS)
        DEALLOCATE(NDAMPPKS,DAMPPKS,DAMPTPKS,RELAXPKS)
        DEALLOCATE(NRESUP)
        DEALLOCATE(HPKS)
C         MILUT PRECONDITIONER
        DEALLOCATE(IFILL)
        DEALLOCATE(NLEVELS)
        DEALLOCATE(DROPTOL)
C         REORDERING
        DEALLOCATE(IORD)
        DEALLOCATE(NIARO)
        DEALLOCATE(NNZRO)
        DEALLOCATE(LORDER)
        DEALLOCATE(IORDER)
        DEALLOCATE(IARO)
        DEALLOCATE(JARO)
        DEALLOCATE(ARO)
C         CONVERGENCE AND TIMING VARIABLES
        DEALLOCATE(IPKSO,IPKSI)
        DEALLOCATE(PKST,PKSPCU,PKSPCA)
        DEALLOCATE(IPRPKS,MUTPKS,IUNITPKS)
        DEALLOCATE(PKSCLEN)
        DEALLOCATE(PKSIT)
        DEALLOCATE(PKSHMXLOC)
        DEALLOCATE(PKSHMX)
        DEALLOCATE(PKSRMXLOC)
        DEALLOCATE(PKSRMX)
        DEALLOCATE(PKSL2NORM)
        DEALLOCATE(PKSRL2NORM)
C         SOLVER VARIABLES
        DEALLOCATE(NODEC)
        DEALLOCATE(BC)
        DEALLOCATE(XC)
        DEALLOCATE(AC)
C        DEALLOCATE(IAC) ! UNCOMMENT FOR STRUCTURED
C        DEALLOCATE(JAC) ! UNCOMMENT FOR STRUCTURED
        DEALLOCATE(IXMAP)
C         PRECONDITIONER        
        DEALLOCATE(APC)
        DEALLOCATE(IAPC)
        DEALLOCATE(JAPC)
        DEALLOCATE(IAPCM)
        DEALLOCATE(JAPCM)
C         WORKING ARRAYS
        DEALLOCATE(IWC)
        DEALLOCATE(WC)
        DEALLOCATE(DC)
        DEALLOCATE(ZC)
        DEALLOCATE(PC)
        DEALLOCATE(QC)
        DEALLOCATE(VC)
        DEALLOCATE(DTILC)
        DEALLOCATE(PHATC)
        DEALLOCATE(DHATC)
        DEALLOCATE(CSG)
        DEALLOCATE(SNG)
        DEALLOCATE(SG)
        DEALLOCATE(YG)
        DEALLOCATE(HG)
        DEALLOCATE(VG)
        DEALLOCATE(ISCL)
        DEALLOCATE(SCL)
        DEALLOCATE(SCLI)
C         SOLUTION VECTORS
        DEALLOCATE(A0)
        DEALLOCATE(IA0)
        DEALLOCATE(JA0)
C
C---------RETURN
      RETURN
      END SUBROUTINE PKS7DA
      
      SUBROUTINE PKS7PNT(IGRID)
C  Set pointers to PKS data for a grid
      USE PKSMODULE
C
#ifdef PKSUNS
      CRDUM=>PKSDAT(IGRID)%CRDUM
      CCDUM=>PKSDAT(IGRID)%CCDUM
      CVDUM=>PKSDAT(IGRID)%CVDUM
      HCOFDUM=>PKSDAT(IGRID)%HCOFDUM
      RHSDUM=>PKSDAT(IGRID)%RHSDUM
#endif      
      ISOLVER=>PKSDAT(IGRID)%ISOLVER
      INNERIT=>PKSDAT(IGRID)%INNERIT
      NPC=>PKSDAT(IGRID)%NPC
      NOPT=>PKSDAT(IGRID)%NOPT
      NCORESM=>PKSDAT(IGRID)%NCORESM
      NCORESV=>PKSDAT(IGRID)%NCORESV
      ICNVGOPT=>PKSDAT(IGRID)%ICNVGOPT
      IEPFACT=>PKSDAT(IGRID)%IEPFACT
      NITERC=>PKSDAT(IGRID)%NITERC
      NNZC=>PKSDAT(IGRID)%NNZC
      NIAC=>PKSDAT(IGRID)%NIAC
      NIABCGS=>PKSDAT(IGRID)%NIABCGS
      NGMRES=>PKSDAT(IGRID)%NGMRES
      NREST=>PKSDAT(IGRID)%NREST
      NIAPC=>PKSDAT(IGRID)%NIAPC
      NJAPC=>PKSDAT(IGRID)%NJAPC
      NNZAPC=>PKSDAT(IGRID)%NNZAPC
      NIAPCM=>PKSDAT(IGRID)%NIAPCM
      NJAPCM=>PKSDAT(IGRID)%NJAPCM
      NIWC=>PKSDAT(IGRID)%NIWC
      NWC=>PKSDAT(IGRID)%NWC
      HCLOSEPKS=>PKSDAT(IGRID)%HCLOSEPKS
      RCLOSEPKS=>PKSDAT(IGRID)%RCLOSEPKS
      NDAMPPKS=>PKSDAT(IGRID)%NDAMPPKS
      DAMPPKS=>PKSDAT(IGRID)%DAMPPKS
      DAMPTPKS=>PKSDAT(IGRID)%DAMPTPKS
      RELAXPKS=>PKSDAT(IGRID)%RELAXPKS
      NRESUP=>PKSDAT(IGRID)%NRESUP
      HPKS=>PKSDAT(IGRID)%HPKS
C       MILUT PRECONDITIONER
      IFILL=>PKSDAT(IGRID)%IFILL
      NLEVELS=>PKSDAT(IGRID)%NLEVELS
      DROPTOL=>PKSDAT(IGRID)%DROPTOL
C       REORDERING
      IORD=>PKSDAT(IGRID)%IORD
      NIARO=>PKSDAT(IGRID)%NIARO
      NNZRO=>PKSDAT(IGRID)%NNZRO
      LORDER=>PKSDAT(IGRID)%LORDER
      IORDER=>PKSDAT(IGRID)%IORDER
      IARO=>PKSDAT(IGRID)%IARO
      JARO=>PKSDAT(IGRID)%JARO
      ARO=>PKSDAT(IGRID)%ARO
C       CONVERGENCE AND TIMING VARIABLES
      IPKSO=>PKSDAT(IGRID)%IPKSO
      IPKSI=>PKSDAT(IGRID)%IPKSI
      PKST=>PKSDAT(IGRID)%PKST
      PKSPCU=>PKSDAT(IGRID)%PKSPCU
      PKSPCA=>PKSDAT(IGRID)%PKSPCA
      IPRPKS=>PKSDAT(IGRID)%IPRPKS
      MUTPKS=>PKSDAT(IGRID)%MUTPKS
      IUNITPKS=>PKSDAT(IGRID)%IUNITPKS
      PKSCLEN=>PKSDAT(IGRID)%PKSCLEN
      PKSIT=>PKSDAT(IGRID)%PKSIT
      PKSHMXLOC=>PKSDAT(IGRID)%PKSHMXLOC
      PKSHMX=>PKSDAT(IGRID)%PKSHMX
      PKSRMXLOC=>PKSDAT(IGRID)%PKSRMXLOC
      PKSRMX=>PKSDAT(IGRID)%PKSRMX
      PKSL2NORM=>PKSDAT(IGRID)%PKSL2NORM
      PKSRL2NORM=>PKSDAT(IGRID)%PKSRL2NORM
C       SOLVER VARIABLES
      NODEC=>PKSDAT(IGRID)%NODEC
      BC=>PKSDAT(IGRID)%BC
      XC=>PKSDAT(IGRID)%XC
      AC=>PKSDAT(IGRID)%AC
C      IAC=>PKSDAT(IGRID)%IAC ! UNCOMMENT FOR STRUCTURED
C      JAC=>PKSDAT(IGRID)%JAC ! UNCOMMENT FOR STRUCTURED
      IXMAP=>PKSDAT(IGRID)%IXMAP
C       PRECONDITIONER      
      APC=>PKSDAT(IGRID)%APC
      IAPC=>PKSDAT(IGRID)%IAPC
      JAPC=>PKSDAT(IGRID)%JAPC
      IAPCM=>PKSDAT(IGRID)%IAPCM
      JAPCM=>PKSDAT(IGRID)%JAPCM
C       WORKING ARRAYS
      IWC=>PKSDAT(IGRID)%IWC
      WC=>PKSDAT(IGRID)%WC
      DC=>PKSDAT(IGRID)%DC
      ZC=>PKSDAT(IGRID)%ZC
      PC=>PKSDAT(IGRID)%PC
      QC=>PKSDAT(IGRID)%QC
      VC=>PKSDAT(IGRID)%VC
      DTILC=>PKSDAT(IGRID)%DTILC
      PHATC=>PKSDAT(IGRID)%PHATC
      DHATC=>PKSDAT(IGRID)%DHATC
      CSG=>PKSDAT(IGRID)%CSG
      SNG=>PKSDAT(IGRID)%SNG
      SG=>PKSDAT(IGRID)%SG
      YG=>PKSDAT(IGRID)%YG
      HG=>PKSDAT(IGRID)%HG
      VG=>PKSDAT(IGRID)%VG
C       SCALING
      ISCL=>PKSDAT(IGRID)%ISCL
      SCL=>PKSDAT(IGRID)%SCL
      SCLI=>PKSDAT(IGRID)%SCLI
C       SOLUTION VECTORS
      A0=>PKSDAT(IGRID)%A0
      IA0=>PKSDAT(IGRID)%IA0
      JA0=>PKSDAT(IGRID)%JA0
C
      RETURN
      END SUBROUTINE PKS7PNT

      SUBROUTINE PKS7PSV(IGRID)
C  Save pointers to PKS data
      USE PKSMODULE
C
#ifdef PKSUNS
      PKSDAT(IGRID)%CRDUM=>CRDUM
      PKSDAT(IGRID)%CCDUM=>CCDUM
      PKSDAT(IGRID)%CVDUM=>CVDUM
      PKSDAT(IGRID)%HCOFDUM=>HCOFDUM
      PKSDAT(IGRID)%RHSDUM=>RHSDUM
#endif      
      PKSDAT(IGRID)%ISOLVER=>ISOLVER
      PKSDAT(IGRID)%INNERIT=>INNERIT
      PKSDAT(IGRID)%NPC=>NPC
      PKSDAT(IGRID)%NOPT=>NOPT
      PKSDAT(IGRID)%NCORESM=>NCORESM
      PKSDAT(IGRID)%NCORESV=>NCORESV
      PKSDAT(IGRID)%ICNVGOPT=>ICNVGOPT
      PKSDAT(IGRID)%IEPFACT=>IEPFACT
      PKSDAT(IGRID)%NITERC=>NITERC
      PKSDAT(IGRID)%NNZC=>NNZC
      PKSDAT(IGRID)%NIAC=>NIAC
      PKSDAT(IGRID)%NIABCGS=>NIABCGS
      PKSDAT(IGRID)%NGMRES=>NGMRES
      PKSDAT(IGRID)%NREST=>NREST
      PKSDAT(IGRID)%NIAPC=>NIAPC
      PKSDAT(IGRID)%NJAPC=>NJAPC
      PKSDAT(IGRID)%NNZAPC=>NNZAPC
      PKSDAT(IGRID)%NIAPCM=>NIAPCM
      PKSDAT(IGRID)%NJAPCM=>NJAPCM
      PKSDAT(IGRID)%NIWC=>NIWC
      PKSDAT(IGRID)%NWC=>NWC
      PKSDAT(IGRID)%HCLOSEPKS=>HCLOSEPKS
      PKSDAT(IGRID)%RCLOSEPKS=>RCLOSEPKS
      PKSDAT(IGRID)%NDAMPPKS=>NDAMPPKS
      PKSDAT(IGRID)%DAMPPKS=>DAMPPKS
      PKSDAT(IGRID)%DAMPTPKS=>DAMPTPKS
      PKSDAT(IGRID)%RELAXPKS=>RELAXPKS
      PKSDAT(IGRID)%NRESUP=>NRESUP
      PKSDAT(IGRID)%HPKS=>HPKS
C       MILUT PRECONDITIONER
      PKSDAT(IGRID)%IFILL=>IFILL
      PKSDAT(IGRID)%NLEVELS=>NLEVELS
      PKSDAT(IGRID)%DROPTOL=>DROPTOL
C       REORDERING
      PKSDAT(IGRID)%IORD=>IORD
      PKSDAT(IGRID)%NIARO=>NIARO
      PKSDAT(IGRID)%NNZRO=>NNZRO
      PKSDAT(IGRID)%LORDER=>LORDER
      PKSDAT(IGRID)%IORDER=>IORDER
      PKSDAT(IGRID)%IARO=>IARO
      PKSDAT(IGRID)%JARO=>JARO
      PKSDAT(IGRID)%ARO=>ARO
C       CONVERGENCE AND TIMING VARIABLES
      PKSDAT(IGRID)%IPKSO=>IPKSO
      PKSDAT(IGRID)%IPKSI=>IPKSI
      PKSDAT(IGRID)%PKST=>PKST
      PKSDAT(IGRID)%PKSPCU=>PKSPCU
      PKSDAT(IGRID)%PKSPCA=>PKSPCA
      PKSDAT(IGRID)%IPRPKS=>IPRPKS
      PKSDAT(IGRID)%MUTPKS=>MUTPKS
      PKSDAT(IGRID)%IUNITPKS=>IUNITPKS
      PKSDAT(IGRID)%PKSCLEN=>PKSCLEN
      PKSDAT(IGRID)%PKSIT=>PKSIT
      PKSDAT(IGRID)%PKSHMXLOC=>PKSHMXLOC
      PKSDAT(IGRID)%PKSHMX=>PKSHMX
      PKSDAT(IGRID)%PKSRMXLOC=>PKSRMXLOC
      PKSDAT(IGRID)%PKSRMX=>PKSRMX
      PKSDAT(IGRID)%PKSL2NORM=>PKSL2NORM
      PKSDAT(IGRID)%PKSRL2NORM=>PKSRL2NORM
C       SOLVER VARIABLES
      PKSDAT(IGRID)%NODEC=>NODEC
      PKSDAT(IGRID)%BC=>BC
      PKSDAT(IGRID)%XC=>XC
      PKSDAT(IGRID)%AC=>AC
C      PKSDAT(IGRID)%IAC=>IAC ! UNCOMMENT FOR STRUCTURED
C      PKSDAT(IGRID)%JAC=>JAC ! UNCOMMENT FOR STRUCTURED
      PKSDAT(IGRID)%IXMAP=>IXMAP
C       PRECONDITIONER      
      PKSDAT(IGRID)%APC=>APC
      PKSDAT(IGRID)%IAPC=>IAPC
      PKSDAT(IGRID)%JAPC=>JAPC
      PKSDAT(IGRID)%IAPCM=>IAPCM
      PKSDAT(IGRID)%JAPCM=>JAPCM
C       WORKING ARRAYS
      PKSDAT(IGRID)%IWC=>IWC
      PKSDAT(IGRID)%WC=>WC
      PKSDAT(IGRID)%DC=>DC
      PKSDAT(IGRID)%ZC=>ZC
      PKSDAT(IGRID)%PC=>PC
      PKSDAT(IGRID)%QC=>QC
      PKSDAT(IGRID)%VC=>VC
      PKSDAT(IGRID)%DTILC=>DTILC
      PKSDAT(IGRID)%PHATC=>PHATC
      PKSDAT(IGRID)%DHATC=>DHATC
      PKSDAT(IGRID)%CSG=>CSG
      PKSDAT(IGRID)%SNG=>SNG
      PKSDAT(IGRID)%SG=>SG
      PKSDAT(IGRID)%YG=>YG
      PKSDAT(IGRID)%HG=>HG
      PKSDAT(IGRID)%VG=>VG
C       SCALING
      PKSDAT(IGRID)%ISCL=>ISCL
      PKSDAT(IGRID)%SCL=>SCL
      PKSDAT(IGRID)%SCLI=>SCLI
C       SOLUTION VECTORS
      PKSDAT(IGRID)%IA0=>IA0
      PKSDAT(IGRID)%JA0=>JA0
      PKSDAT(IGRID)%A0=>A0
C
      RETURN
      END SUBROUTINE PKS7PSV
C
C---------GENERATE IAPC AND JAPC FROM IA AND JA
C         JAPC(1:NIAC) HAS THE POSITION OF THE UPPER ENTRY FOR A ROW
C         JAPC(NIAC+1:NNZC) IS THE COLUMN POSITION FOR ENTRY
C         APC(1:NIAC) PRECONDITIONED INVERSE OF THE DIAGONAL
C         APC(NIAC+1:NNZC) PRECONDITIONED ENTRIES FOR OFF DIAGONALS
        SUBROUTINE SPKS_PCCRS(NIAC,NNZC,IAC,JAC,
     2                        NIAPC,NJAPC,IAPC,JAPC)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN)         :: NIAC
        INTEGER, INTENT(IN)         :: NNZC
        INTEGER, DIMENSION(NIAC+1), INTENT(IN)     :: IAC
        INTEGER, DIMENSION(NNZC), INTENT(IN)       :: JAC
        INTEGER, INTENT(IN)         :: NIAPC
        INTEGER, INTENT(IN)         :: NJAPC
        INTEGER, DIMENSION(NIAPC+1), INTENT(INOUT) :: IAPC
        INTEGER, DIMENSION(NJAPC), INTENT(INOUT)   :: JAPC
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n, j
        INTEGER :: i0, i1
        INTEGER :: nlen
        INTEGER :: ic,ip
        INTEGER :: jcol
        INTEGER, DIMENSION(:), ALLOCATABLE :: iarr
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        ip = NIAC + 1
        DO n = 1, NIAC
          i0 = IAC(n)
          i1 = IAC(n+1) - 1
          nlen = i1 - i0
          ALLOCATE( iarr(nlen) )
          ic = 0
          DO j = i0, i1
            jcol = JAC(j)
            IF ( jcol.EQ.n ) CYCLE
            ic = ic + 1
            iarr(ic) = jcol
          END DO
          CALL SPKS_ISORT(nlen,iarr)
          IAPC(n) = ip
          DO j = 1, nlen
            jcol = iarr(j)
            JAPC(ip) = jcol
            ip = ip + 1
          END DO
          DEALLOCATE(iarr)
        END DO
        IAPC(NIAC+1) = NNZC + 1
C---------POSITION OF THE FIRST UPPER ENTRY FOR ROW         
        DO n = 1, NIAC
          i0 = IAPC(n)
          i1 = IAPC(n+1) - 1
          JAPC(n) = IAPC(n+1)
          DO j = i0, i1
            jcol = JAPC(j)
            IF ( jcol.GT.n ) THEN
              JAPC(n) = j
              EXIT
            END IF
          END DO
        END DO
C---------RETURN
        RETURN
      END SUBROUTINE SPKS_PCCRS
C
C-------ROUTINE TO SCALE THE COEFFICIENT MATRIX
      SUBROUTINE SPKS_SCL(ISCALE,NNZC,NIAC,AC,XC,BC,SCL,SCLI,IAC,JAC,
     &                    NODEC)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: ISCALE
        INTEGER, INTENT(IN) :: NNZC
        INTEGER, INTENT(IN) :: NIAC
        DOUBLEPRECISION, DIMENSION(NNZC),  INTENT(INOUT)  :: AC
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(INOUT)  :: XC
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(INOUT)  :: BC
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(INOUT)  :: SCL
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(INOUT)  :: SCLI
        INTEGER, DIMENSION(NIAC+1), INTENT(IN)   :: IAC
        INTEGER, DIMENSION(NNZC), INTENT(IN)     :: JAC
        INTEGER, DIMENSION(*), INTENT(IN) :: NODEC
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n, i
        INTEGER :: ic
        INTEGER :: i0, i1
        DOUBLEPRECISION :: c1, c2, v
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
C
C---------SCALE AC, XC, AND BC
        IF ( ISCALE.NE.0 ) THEN
          DO n = 1, NIAC
            ic = IAC(n)
            SCL(n)  = 1.0D0 / SQRT( ABS( AC(ic) ) )
          END DO
          CALL PKS7MPILXCHDUNS(SCL, NODEC)                              !JV
          DO n = 1, NIAC
            ic = IAC(n)
            SCLI(n) = 1.0D0/SCL(n)
          END DO
          DO n = 1, NIAC
            c1 = SCL(n)
            i0 = IAC(n)
            i1 = IAC(n+1) - 1
            DO i = i0, i1
              ic    = JAC(i)
              c2    = SCL(ic)
              v     = c1 * AC(i) * c2
              AC(i) = v
            END DO
          END DO
C-----------SCALE XC AND BC
          DO n = 1, NIAC
            c1     = SCL(n)
            c2     = SCLI(n)
            XC(n)  = XC(n) * c2
            BC(n)  = BC(n) * c1
          END DO
C---------UNSCALE XC -- NO NEED TO UNSCALE AC AND BC BECAUSE THEY ARE NOT REUSED
        ELSE
          DO n = 1, NIAC
            c1 = SCL(n)
            i0 = IAC(n)
            i1 = IAC(n+1) - 1
!C             UNSCALE AC
!            DO i = i0, i1
!              jc = JAC(i)
!              c2 = SCL(jc)
!              AC(i) = ( 1.0D0 / c1 ) * AC(i) * ( 1.0D0 / c2 ) 
!            END DO
C             UNSCALE XC
            XC(n) = XC(n) * c1
!            c2 = SCLI(n)
!            BC(n) = BC(n) * c2
          END DO     
        END IF
C---------RETURN
        RETURN
      END SUBROUTINE SPKS_SCL
C
C-------ROUTINE TO UPDATE THE PRECONDITIONER
      SUBROUTINE SPKS_PCU(IOUT,NCORESM,NCORESV,NNZC,NIAC,
     M                     NIAPC,NJAPC,NNZAPC,NIAPCM,NJAPCM,NIWC,NWC,
     M                     NPC,RELAX,IFILL,NLEVELS,DROPTOL,
     M                     AC,IAC,JAC,APC,IAPC,JAPC,IAPCM,JAPCM,
     M                     IWC,WC,
     M                     IXMAP,NODES)                                 !JV
      
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: IOUT
        INTEGER, INTENT(IN) :: NCORESM
        INTEGER, INTENT(IN) :: NCORESV
        INTEGER, INTENT(IN) :: NNZC
        INTEGER, INTENT(IN) :: NIAC
        INTEGER, INTENT(IN) :: NIAPC
        INTEGER, INTENT(IN) :: NJAPC
        INTEGER, INTENT(IN) :: NNZAPC
        INTEGER, INTENT(IN) :: NIAPCM
        INTEGER, INTENT(IN) :: NJAPCM
        INTEGER, INTENT(IN) :: NIWC
        INTEGER, INTENT(IN) :: NWC
        INTEGER, INTENT(IN) :: NPC
        REAL, INTENT(IN)    :: RELAX
        INTEGER, INTENT(IN) :: IFILL
        INTEGER, INTENT(IN) :: NLEVELS
        REAL, INTENT(IN)    :: DROPTOL
        DOUBLEPRECISION, DIMENSION(NNZC), INTENT(IN)     :: AC
        INTEGER, DIMENSION(NIAC+1), INTENT(IN)   :: IAC
        INTEGER, DIMENSION(NNZC), INTENT(IN)     :: JAC
        DOUBLEPRECISION, DIMENSION(NNZAPC), INTENT(INOUT) :: APC
        INTEGER, DIMENSION(NIAPC+1), INTENT(INOUT) :: IAPC
        INTEGER, DIMENSION(NNZAPC), INTENT(INOUT)  :: JAPC
        INTEGER, DIMENSION(NIAPCM+1), INTENT(IN)   :: IAPCM
        INTEGER, DIMENSION(NJAPCM), INTENT(IN)     :: JAPCM
        INTEGER, DIMENSION(NIWC), INTENT(INOUT)  :: IWC
        DOUBLEPRECISION, DIMENSION(NWC), INTENT(INOUT)  :: WC
        INTEGER, DIMENSION(NIAC), INTENT(IN)     :: IXMAP               !JV
        INTEGER, INTENT(IN)                      :: NODES               !JV
C     + + + LOCAL DEFINITIONS + + +
        DOUBLEPRECISION :: drelax
        DOUBLEPRECISION :: ddroptol
        DOUBLEPRECISION :: delta
        INTEGER :: izero
        integer :: n, i0, i1, j
C     + + + FUNCTIONS + + +
C     + + + FORMATS + + +
2000    FORMAT (/,' MATRIX IS SEVERELY NON-DIAGONALLY DOMINANT.',
     &          /,' ADDING SMALL VALUE TO PIVOT (SPKS_PCU)')
C     + + + CODE + + +
        drelax   = DBLE( RELAX )
        ddroptol = DBLE( DROPTOL )
        delta = 0.0D0
        izero = 0
        LUPC: DO
          SELECT CASE(NPC)
C             JACOBI            
            CASE (0)
              CALL SPKS_JAC(NCORESV,NNZC,NIAC,AC,IAC,APC,
     4                      IXMAP,NODES)                                !JV
C             MIC0 AND MILU0
            CASE (1,2)
              IF ( NPC.EQ.1 ) THEN
C                CALL SPKS_PCCRS(NIAC,NNZC,IAC,JAC,
C     2                          NIAPC,NJAPC,IAPC,JAPC)
                CALL SPKS_ICOPY(NCORESV, NIAPCM+1, IAPCM, IAPC)
                CALL SPKS_ICOPY(NCORESV, NJAPCM, JAPCM, JAPC)
              END IF
              CALL SPKS_MILU0(NCORESV,NNZC,NIAC,NIWC,NWC,
     M                        AC,IAC,JAC,
     M                        APC,IAPC,JAPC,IWC,WC,
     M                        drelax,delta,izero,
     M                        IXMAP,NODES)                              !JV
              
C            MILUT
            CASE (3)
              CALL SPKS_MILUT(NIAC,NNZC,NNZAPC,NIWC,NWC,
     M                        AC,IAC,JAC,
     M                        drelax,delta,IFILL,NLEVELS,ddroptol,
     M                        APC,IAPC,JAPC,IWC,WC,izero,
     M                        IXMAP,NODES)                              !JV
C             ADDITIONAL PRECONDITIONERS
          END SELECT
          IF ( izero.LT.1 ) THEN
            EXIT LUPC
          ENDIF
          delta = 1.5D0 * delta + 0.001
          izero = 0
          IF ( delta.GT.0.5D0 ) THEN
            WRITE(IOUT,2000)
            delta = 0.5D0
            izero = 2
          END IF
        END DO LUPC
C         CONVERT MILU0 TO MIC0, IF REQUIRED
        IF ( NPC.EQ.1 ) THEN
          CALL SPKS_ILU02MIC0(NNZC,NIAC,APC,IAC,JAC,IAPC,JAPC,
     1     IXMAP,NODES)                                                 !JV
        END IF
C---------RETURN
        RETURN
      END SUBROUTINE SPKS_PCU

      SUBROUTINE SPKS_PCA(NCORESV,NPC,NNZC,NIAC,
     2                    NIAPC,NJAPC,NNZAPC,
     3                    APC,IAC,JAC,IAPC,JAPC,
     4                    DC,ZC,IXMAP,NODES)                            !JV
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NCORESV
        INTEGER, INTENT(IN) :: NPC
        INTEGER, INTENT(IN) :: NNZC
        INTEGER, INTENT(IN) :: NIAC
        INTEGER, INTENT(IN) :: NIAPC
        INTEGER, INTENT(IN) :: NJAPC
        INTEGER, INTENT(IN) :: NNZAPC
        DOUBLEPRECISION, DIMENSION(NNZAPC), INTENT(INOUT)  :: APC
        INTEGER, DIMENSION(NIAC+1), INTENT(IN)   :: IAC
        INTEGER, DIMENSION(NNZC), INTENT(IN)     :: JAC
        INTEGER, DIMENSION(NIAPC+1), INTENT(IN)  :: IAPC
        INTEGER, DIMENSION(NJAPC), INTENT(IN)    :: JAPC
        DOUBLEPRECISION, DIMENSION(NIAC), INTENT(IN)    :: DC
        DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT) :: ZC
        INTEGER, DIMENSION(NIAC), INTENT(IN)     :: IXMAP               !JV
        INTEGER, INTENT(IN) :: NODES                                    !JV
C     + + + LOCAL DEFINITIONS + + +
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        SELECT CASE (NPC)
C           JACOBI PRECONDITIONER
          CASE (0)
            CALL SPKS_JACA(NCORESV,NIAC,APC,DC,ZC,IXMAP,NODES)
C           MIC0 PRECONDITIONER
          CASE (1)
            CALL SPKS_MIC0A(NCORESV,NNZAPC,NIAPC,
     2                      APC,IAPC,JAPC,DC,ZC,IXMAP,NODES)            !JV
C           ILU0 AND MILU0 PRECONDITIONERS
          CASE (2,3)
            CALL SPKS_MILUTA(NIAC,NNZAPC,
     M                       APC,IAPC,JAPC,DC,ZC,IXMAP,NODES)           !JV
!            CALL SPKSILU0A(NNZC,NIAC,
!     2                      APC,IAC,JAC,IUC,DC,ZC,IXMAP,NODES)          !JV
          END SELECT
C---------RETURN
        RETURN
      END SUBROUTINE SPKS_PCA

      SUBROUTINE SPKS_JAC(NCORESV,NNZC,NIAC,AC,IAC,APC,
     2                     IXMAP,NODES)                                 !JV
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NCORESV
        INTEGER, INTENT(IN) :: NNZC
        INTEGER, INTENT(IN) :: NIAC
        DOUBLEPRECISION, DIMENSION(NNZC),  INTENT(IN)   :: AC
        INTEGER, DIMENSION(NIAC+1), INTENT(IN)   :: IAC
        DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT) :: APC
        INTEGER, DIMENSION(NIAC), INTENT(IN) :: IXMAP                   !JV
        INTEGER, INTENT(IN) :: NODES                                    !JV
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
        INTEGER :: id0
        DOUBLEPRECISION :: tl
C     + + + PARAMETERS + + +
        DOUBLEPRECISION, PARAMETER :: DZERO = 0.0D0
        DOUBLEPRECISION, PARAMETER :: DONE  = 1.0D0
        DOUBLEPRECISION, PARAMETER :: DTINY = 1.0D-06
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        MAIN: DO n = 1, NIAC
          id0 = IAC(n)                                                 
          IF (IXMAP(n).LT.-NODES) THEN
            APC(n) = DONE                                               !JV
            CYCLE MAIN
          END IF
          tl = AC(id0)
          IF ( tl.EQ.DZERO ) THEN
            tl = DSIGN(DTINY,tl)
          END IF
          APC(n) = DONE / tl
        END DO MAIN
C---------RETURN
        RETURN
      END SUBROUTINE SPKS_JAC
      
      SUBROUTINE SPKS_JACA(NCORESV,NIAC,APC,R,D,IXMAP,NODES)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NCORESV
        INTEGER, INTENT(IN) :: NIAC
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(IN)    :: APC
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(IN)    :: R
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(INOUT) :: D
        INTEGER, DIMENSION(NIAC), INTENT(IN) :: IXMAP                   !JV
        INTEGER, INTENT(IN) :: NODES                                    !JV
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
        DOUBLEPRECISION :: tv
C     + + + PARAMETERS + + + 
        DOUBLEPRECISION, PARAMETER :: DZERO = 0.0D0
        DOUBLEPRECISION, PARAMETER :: DONE  = 1.0D0
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        CALL SPKS_VVMULT(NCORESV, NIAC, APC, R, D)
        !SHOULD IXMAP AND NODES BE PASSED TO SPKS_VVMULT??
        !DO n = 1, NIAC
        !  IF ( IXMAP(n).LT.-NODES ) CYCLE                               !JV
        !  tv     = APC(n) * R(n)
        !  D(n) = tv
        !END DO
C---------RETURN
        RETURN
      END SUBROUTINE SPKS_JACA
C     
	SUBROUTINE SPKS_MILUTA(NIAC,NNZAPC,
     M                       APC,IAPC,JAPC,R,D,IXMAP,NODES)
C-----------------------------------------------------------------------
C
C THIS ROUTINE SOLVES THE SYSTEM (LU) D = R, 
C GIVEN AN LU DECOMPOSITION OF A MATRIX STORED IN (APC, JAPC, IAPC) 
C MODIFIED SPARSE ROW FORMAT 
C
C-----------------------------------------------------------------------
C ON ENTRY:
C NIAC = DIMENSION OF SYSTEM 
C R    = THE RIGHT-HAND-SIDE VECTOR
C APC, JAPC, IAPC 
C      = THE LU MATRIX AS PROVIDED FROM THE ILU ROUTINES. 
C
C ON RETURN
C D   = SOLUTION OF LU D = R.     
C-----------------------------------------------------------------------
C 
C NOTE: ROUTINE IS IN PLACE: 
C       CALL PKS_MILUTSOL(NIAC,NNZAPC,APC,IAPC,JAPC,R,D) 
C       WILL SOLVE THE SYSTEM WITH RHS R AND OVERWRITE THE RESULT ON D . 
C
C-----------------------------------------------------------------------
C
C        SPECIFICATIONS:
C-----------------------------------------------------------------------
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NIAC
        INTEGER, INTENT(IN) :: NNZAPC
        DOUBLEPRECISION, DIMENSION(NNZAPC), INTENT(IN)   :: APC
        INTEGER, DIMENSION(NIAC+1), INTENT(IN) :: IAPC
        INTEGER, DIMENSION(NNZAPC), INTENT(IN) :: JAPC
        DOUBLEPRECISION, DIMENSION(NIAC), INTENT(IN)    :: R
        DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT) :: D
        INTEGER, DIMENSION(NIAC), INTENT(IN) :: IXMAP                   !JV
        INTEGER, INTENT(IN) :: NODES                                    !JV
C       + + + LOCAL DEFINITIONS + + +
        INTEGER :: ic0, ic1
        INTEGER :: iu
        INTEGER :: jcol
        INTEGER :: j, n
        DOUBLEPRECISION :: tv
C       + + + FUNCTIONS + + +
C       + + + FORMATS + + +
C       + + + DATA + + +
C       + + + CODE + + +
C
C         FORWARD SOLVE - APC * D = R
        FORWARD: DO n = 1, NIAC
!          IF ( IXMAP(n).LT.-NODES ) CYCLE
          tv   = R(n)
!          IF ( IXMAP(n).LT.-NODES ) tv = 0.d0                           ! JV
          ic0 = IAPC(n)
          ic1 = IAPC(n+1) - 1
          iu  = JAPC(n) - 1
          LOWER: DO j = ic0, iu
            jcol = JAPC(j)
            tv    = tv - APC(j) * D(jcol)
          END DO LOWER
          D(n) = tv
        END DO FORWARD
C         BACKWARD SOLVE - D = D / U
        BACKWARD: DO n = NIAC, 1, -1
!          IF ( IXMAP(n).LT.-NODES ) CYCLE
          ic0 = IAPC(n)
          ic1 = IAPC(n+1) - 1
          iu  = JAPC(n)
          tv   = D(n)
          UPPER: DO j = iu, ic1 
            jcol = JAPC(j)
            tv    = tv - APC(j) * D(jcol)
          END DO UPPER
C           COMPUTE D FOR DIAGONAL - D = D / U
          D(n) =  tv * APC(n)          
        END DO BACKWARD
C---------RETURN
        RETURN
      END SUBROUTINE SPKS_MILUTA
C     
C-------INPLACE CONVERSION OF L AND U FROM ILU0 TO L FOR MIC0
      SUBROUTINE SPKS_ILU02MIC0(NNZC,NIAC,APC,IAC,JAC,IAPC,JAPC,
     2                          IXMAP,NODES)                            !JV
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NNZC
        INTEGER, INTENT(IN) :: NIAC
        INTEGER, DIMENSION(NIAC+1), INTENT(IN)   :: IAC
        INTEGER, DIMENSION(NNZC), INTENT(IN)     :: JAC
        DOUBLEPRECISION, DIMENSION(NNZC), INTENT(INOUT) :: APC
        INTEGER, DIMENSION(NIAC+1), INTENT(INOUT) :: IAPC
        INTEGER, DIMENSION(NNZC), INTENT(INOUT)   :: JAPC
        INTEGER, DIMENSION(NIAC), INTENT(IN) :: IXMAP                   !JV
        INTEGER, INTENT(IN) :: NODES                                    !JV
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: ic0, ic1
        INTEGER :: iz
        INTEGER :: j, n
        INTEGER :: jcol
        DOUBLEPRECISION, PARAMETER :: DZERO = 0.0D0
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        iz = 1
        IAPC(1) = NIAC+2
        MAIN: DO n = 1, NIAC
          ic0 = IAC(n) + 1
          ic1 = IAC(n) - 1
          DO j = ic0, ic1
            jcol = JAC(j)
            IF ( jcol.GT.n ) CYCLE
            JAPC(iz) = jcol
            APC(iz) = APC(j)
            iz = iz + 1
          END DO
          IAPC(n+1) = iz
        END DO MAIN
!        UPPERZERO: DO j = iz, NNZC
!          APC(j) = DZERO
!        END DO UPPERZERO
C---------RETURN
        RETURN
      END SUBROUTINE SPKS_ILU02MIC0

      
      SUBROUTINE SPKS_MIC0A(NCORESV,NNZC,NIAC,
     2                      APC,IAPC,JAPC,R,D,
     3                      IXMAP,NODES)                                !JV
        use pksmpi_mod, only: myrank
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NCORESV
        INTEGER, INTENT(IN) :: NNZC
        INTEGER, INTENT(IN) :: NIAC
        DOUBLEPRECISION, DIMENSION(NNZC),  INTENT(INOUT)  :: APC
        INTEGER, DIMENSION(NIAC+1), INTENT(IN)  :: IAPC
        INTEGER, DIMENSION(NNZC), INTENT(IN)    :: JAPC
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(IN)     :: R
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(INOUT)  :: D
        INTEGER, DIMENSION(NIAC),  INTENT(IN) :: IXMAP                  !JV
        INTEGER, INTENT(IN) :: NODES                                    !JV
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: ic0, ic1
        INTEGER :: jcol
        INTEGER :: j, n
        DOUBLEPRECISION :: t
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
C         FORWARD SOLVE - L * y = b - STORE RESULT IN D
        FORWARD: DO n = 1, NIAC
          t   = R(n)
          ic0 = IAPC(n)
          ic1 = IAPC(n+1) - 1
          LOWER: DO j = ic0, ic1
            jcol = JAPC(j)
            t    = t - APC(j) * D(jcol)
          END DO LOWER
          D(n) = t
        END DO FORWARD
C         SOLVE - DIAG * Z = Y - STORE RESULTS IN D
        DO n = 1, NIAC
          D(n) = D(n) * APC(n) 
        END DO
C         BACKWARD SOLVE - L-trans * X = Z - STORE RESULTS IN D
        BACKWARD: DO n = NIAC, 2, -1
          ic0 = IAPC(n)
          ic1 = IAPC(n+1) - 1
          UPPER: DO j = ic0, ic1
            jcol = JAPC(j)
            D(jcol) = D(jcol) - APC(j) * D(n)
          END DO UPPER
        END DO BACKWARD
C---------RETURN
        RETURN
      END SUBROUTINE SPKS_MIC0A
      
      SUBROUTINE SPKS_MILUT(NIAC,NNZC,NNZAPC,NIWC,NWC,AC,IAC,JAC,
     M                     DRELAX,DELTA,IFILL,NLEVELS,DROPTOL,
     M                     APC,IAPC,JAPC,IWC,WC,IZERO,
     M                     IXMAP,NODES)
C----------------------------------------------------------------------*
C                      *** ILUT PRECONDITIONER ***                     *
C      INCOMPLETE LU FACTORIZATION WITH DUAL TRUNCATION MECHANISM      *
C----------------------------------------------------------------------*
C     AUTHOR: YOUSEF SAAD *MAY, 5, 1990, LATEST REVISION, AUGUST 1996  *
C----------------------------------------------------------------------*
C    MODFIED: FOR PKS BY J.D. HUGHES
C             PRECONDITIONER STORAGE FORMAT MODIFIED -
C             SEE DESCRIPTION OF VARIABLES LISTED BELOW
C----------------------------------------------------------------------*
C PARAMETERS                                                           
C-----------                                                           
C
C ON ENTRY:
C========== 
C NIAC    = INTEGER. THE ROW DIMENSION OF THE AC VECTOR (THE TOTAL 
C           NUMBER OF EQUATIONS). 
C
C NNZC    = INTEGER. THE LENGTH OF THE AC AND JAC VECTORS.
C
C NNZAPC  = INTEGER. THE LENGTH OF THE APC AND JAPC VECTORS.
C
C NIWC    = INTEGER. THE LENGTH OF THE IWC VECTOR (2xNIAC).
C
C NWC     = INTEGER. THE LENGTH OF THE WC VECTOR (NIAC+1).
C
C AC IS STORED IN COMPRESSED SPARSE ROW FORMAT.              
C
C AC      = DOUBLE. COEFFICIENT VECTOR (NNZC).
C
C IAC     = INTEGER. ROW POINTER VECTOR (NIAC+1).
C
C JAC     = INTEGER. COLUMN POINTER VECTOR (NNZC).
C
C DRELAX  = DOUBLE. RELAXATION FACTOR FOR ROW SUM AGREEMENT. SETTING
C           RELAX=0.0 RESULTS IN ILUT. SETTING RELAX=1.0 RESULTS IN
C           FULL MILUT.
C
C DRELAX  = DOUBLE. DIAGONAL SCALING FACTOR TO REDUCE THE LIKELYHOOD
C           THAT APPLICATION OF ROW SUM AGREEMENT CORRECTIONS WILL
C           RESULT IN ZERO APC DIAGONAL OR A SIGN CHANGE OF APC 
C           DIAGONAL.
C
C NLEVELS = INTEGER. THE FILL-IN PARAMETER. EACH ROW OF L AND EACH ROW
C           OF U WILL HAVE A MAXIMUM OF NLEVELS ELEMENTS (EXCLUDING THE 
C           DIAGONAL ELEMENT). NLEVELS MUST BE .GE. 0.
C
C DROPTOL = DOUBLE. SETS THE THRESHOLD FOR DROPPING SMALL TERMS IN THE
C           FACTORIZATION. SEE BELOW FOR DETAILS ON DROPPING STRATEGY.
C  
C NNZAPC  = INTEGER. THE LENGTHS OF ARRAYS APC AND JAPC. IF THE ARRAYS
C           ARE NOT BIG ENOUGH TO STORE THE ILU FACTORIZATIONS, ILUT
C           WILL STOP WITH AN ERROR MESSAGE. 
C
C IXMAP   = INTEGER. MAPPING VECTOR (NIAC) USED FOR MPI.
C
C NODES   = INTEGER. NUMBER OF NODES IN THE SIMULATION.
C
C ON RETURN:
C===========
C
C APC     = DOUBLE. PRECONDITIONED COEFFICIENT VECTOR (NNZAPC) STORED IN 
C           MODIFIED SPARSE ROW (MSR) FORMAT. THE DIAGONAL (STORED IN 
C           APC(1:NIAC)) IS INVERTED. OFF-DIAGONALS ARE STORED IN 
C           APC(NIAC+1:NNZAPC).
C
C IAPC    = INTEGER. ROW POINTER VECTOR (NIAPC+1) TO THE BEGINNING OF 
C           THE OFF-DIAGONALS FOR EACH ROW IN THE APC VECTOR.
C
C JAPC    = INTEGER. POINTERS TO THE POSITION OF THE FIRST UPPER ENTRY FOR
C           EACH ROW IN THE APC VECTOR STORED IN JAPC(1:NIAPC). COLUMN 
C           POINTERS FOR THE OFF-DIAGONALS ARE STORED IN JAPC(NIAPC+1,NNZAPC).
C
C WORK ARRAYS:
C=============
C IWC     = INTEGER. WORK VECTOR (2xNIAC). IWC(NIAC+1:2N)  STORES NONZERO 
C           INDICATORS
C
C WC      = DOUBLE. WORK VECTOR (NIAC+1). (1:NIAC) STORE THE WORKING ARRAY 
C           [1:II-1 = L-PART, II:NIAC = U] 
C  
C----------------------------------------------------------------------
C 
C NOTES:
C ------
C THE DIAGONAL ELEMENTS OF THE INPUT MATRIX MUST BE  NONZERO (AT LEAST
C 'STRUCTURALLY'). 
C
C-------------------------------------------------------------------------* 
C---- DUAL DROP STRATEGY WORKS AS FOLLOWS.                                *
C                                                                         *
C     1) THERESHOLDING IN L AND U AS SET BY DROPTOL. ANY ELEMENT WHOSE    *
C        MAGNITUDE IS LESS THAN SOME TOLERANCE (RELATIVE TO THE ABS       *
C        VALUE OF DIAGONAL ELEMENT IN U) IS DROPPED.                      *
C                                                                         *
C     2) KEEPING ONLY THE LARGEST NLEVELS ELEMENTS IN THE I-TH ROW OF L   * 
C        AND THE LARGEST NLEVELS ELEMENTS IN THE I-TH ROW OF U (EXCLUDING *
C        DIAGONAL ELEMENTS).                                              *
C                                                                         *
C FLEXIBILITY: ONE  CAN USE  DROPTOL=0  TO GET  A STRATEGY  BASED ON      *
C KEEPING  THE LARGEST  ELEMENTS IN  EACH ROW  OF L  AND U.   TAKING      *
C DROPTOL.NE.0 BUT NLEVELS=NIAC WILL GIVE  THE USUAL THRESHOLD STRATEGY   *
C (HOWEVER, FILL-IN IS THEN UNPREDICTIBLE).                               *
C-------------------------------------------------------------------------*
C        SPECIFICATIONS:
C-----------------------------------------------------------------------
        IMPLICIT NONE 
C       + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NIAC 
        INTEGER, INTENT(IN) :: NNZC 
        INTEGER, INTENT(IN) :: NNZAPC 
        INTEGER, INTENT(IN) :: NIWC 
        INTEGER, INTENT(IN) :: NWC 
        DOUBLEPRECISION, DIMENSION(NNZC), INTENT(IN) ::  AC
        INTEGER, DIMENSION(NIAC+1), INTENT(IN) :: IAC
        INTEGER, DIMENSION(NNZC), INTENT(IN) :: JAC
        DOUBLEPRECISION, INTENT(IN) :: DRELAX
        INTEGER, INTENT(IN) :: IFILL
        INTEGER, INTENT(IN) :: NLEVELS
        DOUBLEPRECISION, INTENT(IN) :: DROPTOL
        DOUBLEPRECISION, DIMENSION(NNZAPC), INTENT(INOUT) ::  APC
        INTEGER, DIMENSION(NIAC+1), INTENT(INOUT) :: IAPC
        INTEGER, DIMENSION(NNZAPC), INTENT(INOUT) :: JAPC
        INTEGER, DIMENSION(NIWC), INTENT(INOUT) :: IWC
        DOUBLEPRECISION, DIMENSION(NWC), INTENT(INOUT) ::  WC
        DOUBLEPRECISION, INTENT(IN) :: DELTA
        INTEGER, INTENT(INOUT) :: IZERO 
        INTEGER, DIMENSION(NIAC), INTENT(IN) :: IXMAP                   !JV
        INTEGER, INTENT(IN) :: NODES                                    !JV
C       + + + LOCAL DEFINITIONS + + +
        INTEGER :: iapc0
        INTEGER :: i, j, k
        INTEGER :: jj, kk
        INTEGER :: n
        INTEGER :: ic0, ic1
        INTEGER :: iic0, iic1
        INTEGER :: jrow, jcol
        INTEGER :: jpos
        INTEGER :: len_lower, len_upper, len_total 
        DOUBLEPRECISION :: tnorm, t, s, fact 
        DOUBLEPRECISION :: tl, rs
        DOUBLEPRECISION :: d, sd1
        INTEGER, DIMENSION(:), ALLOCATABLE :: iu
C       + + + PARAMETERS + + +
        DOUBLEPRECISION, PARAMETER :: DZERO = 0.0D0
        DOUBLEPRECISION, PARAMETER :: DONE  = 1.0D0
        DOUBLEPRECISION, PARAMETER :: DTINY = 1.0D-6
C       + + + FUNCTIONS + + +
C       + + + FORMATS + + +
C       + + + DATA + + +
C       + + + CODE + + +
C
C---------INITIALIZE IAPC0 (POINTS TO NEXT ELEMENT TO BE ADDED TO APC,JAPC)
C         AND POINTER ARRAY.
        ALLOCATE(iu(NIAC))
        iapc0 = NIAC + 1
        IAPC(1) = iapc0
        ic0 = IAC(1) + 1
        ic1 = IAC(2) - 1
        JAPC(1) = iapc0
        iu(1) = iapc0
        DO k = ic0, ic1
          jcol = JAC(k)
          IF ( jcol.GT.1 ) THEN
            EXIT
          END IF
          JAPC(1) = JAPC(1) + 1
          IU(1) = IU(1) + 1
        END DO
C
C---------INITIALIZE NONZERO INDICATOR ARRAY. 
        DO n = 1, NIAC
          IWC(NIAC+n)  = 0
        END DO
C
C---------BEGINNING OF MAIN LOOP.
        MAINL: DO n = 1, NIAC
          ic0 = IAC(n)
          ic1 = IAC(n+1) - 1
          tnorm = DZERO
          DO k = ic0, ic1
            tnorm = tnorm + DABS( AC(k) )
          END DO
C-----------ZERO ROW ENCOUNTERED
          IF ( tnorm.EQ.DZERO ) THEN
            CALL USTOP('ZERO ROW ENCOUNTERED')
          END IF
          tnorm = tnorm / DBLE( ic1 - ic0 + 1 )
C     
C-----------UNPACK L-PART AND U-PART OF ROW OF AC IN ARRAYS WC 
          len_upper = 1
          len_lower = 0
          IWC(n) = n
          WC(n) = DZERO
          IWC(NIAC+n) = n
C
          DO j = ic0, ic1
            jcol = JAC(j)
            t = AC(j)
            IF ( jcol.LT.n ) THEN
              len_lower = len_lower + 1
              IWC(len_lower) = jcol
              WC(len_lower) = t
              IWC(NIAC+jcol) = len_lower
            ELSE IF ( jcol.EQ.n ) THEN
              WC(n) = t
            ELSE
              len_upper = len_upper + 1
              jpos = n + len_upper - 1 
              IWC(jpos) = jcol
              WC(jpos) = t
              IWC(NIAC+jcol) = jpos
            ENDIF
          END DO
          jj = 0
          len_total = 0 
C     
C-----------ELIMINATE PREVIOUS ROWS
 150      jj = jj + 1
          IF ( jj.GT.len_lower ) GO TO 160
C----------------------------------------------------------------------------
C         IN ORDER TO DO THE ELIMINATION IN THE CORRECT ORDER WE MUST SELECT
C         THE SMALLEST COLUMN INDEX AMONG IWC(K), K=JJ+1, ..., len_lower.
C----------------------------------------------------------------------------
          jrow = IWC(jj)
          k = jj
C     
C-----------DETERMINE SMALLEST COLUMN INDEX
          DO j = jj+1, len_lower
            IF ( IWC(j).LT.jrow ) THEN
              jrow = IWC(j)
              k = j
            END IF
          END DO
C
          IF ( k.NE.jj ) THEN
C-------------EXCHANGE IN IWC
            j = IWC(jj)
            IWC(jj) = IWC(k)
            IWC(k) = j
C-------------EXCHANGE IN JR
            IWC(NIAC+jrow) = jj
            IWC(NIAC+j) = k
C-------------EXCHANGE IN WC
            s = WC(jj)
            WC(jj) = WC(k)
            WC(k) = s
          END IF
C
C-----------ZERO OUT ELEMENT IN ROW BY SETTING IWC(NIAC+JROW) TO ZERO.
          IWC(NIAC+jrow) = 0
C
C-----------GET THE MULTIPLIER FOR ROW TO BE ELIMINATED (JROW).
          fact = WC(jj) * APC(jrow)
          IF ( DABS(fact).LE.DROPTOL ) GO TO 150
C     
C-----------COMBINE CURRENT ROW AND ROW JROW
          !iic0 = iu(jrow)
          iic0 = JAPC(jrow)
          iic1 = IAPC(jrow+1) - 1
          DO kk = iic0, iic1
            s = fact * APC(kk)
            j = JAPC(kk)
            jpos = IWC(NIAC+j)
            IF ( j.GE.n ) THEN
C     
C-------------DEALING WITH UPPER PART.
C---------------THIS IS A FILL-IN ELEMENT
              IF ( jpos.EQ.0 ) THEN
                IF ( IFILL.NE.0 ) THEN
                  len_upper = len_upper + 1
C-------------------INCOMPREHENSIBLE ERROR. MATRIX MUST BE WRONG.
                  IF ( len_upper.GT.NIAC ) THEN
                    write (*,*) len_upper, niac
                    CALL USTOP('UPPER LENGTH GREATER THAN NIAC')
                  END IF
                  i = n + len_upper - 1
                  IWC(i) = j
                  IWC(NIAC+j) = i
                  WC(i) = - s
                END IF
C---------------THIS IS NOT A FILL-IN ELEMENT 
              ELSE
                WC(jpos) = WC(jpos) - s
              END IF
            ELSE
C     
C-----------DEALING WITH LOWER PART.
C---------------THIS IS A FILL-IN ELEMENT
              IF ( jpos.EQ.0 ) THEN
                IF ( IFILL.NE.0 ) THEN
                  len_lower = len_lower + 1
C-------------------INCOMPREHENSIBLE ERROR. MATRIX MUST BE WRONG.
                  IF ( len_lower.GT.NIAC ) THEN
                    write (*,*) len_lower, niac
                    CALL USTOP('LOWER LENGTH GREATER THAN NIAC')
                  END IF
                  IWC(len_lower) = j
                  IWC(NIAC+j) = len_lower
                  WC(len_lower) = - s
                END IF
C---------------THIS IS NOT A FILL-IN ELEMENT 
              ELSE
                WC(jpos) = WC(jpos) - s
              END IF
            END IF
          END DO
C     
C-----------STORE THIS PIVOT ELEMENT -- (FROM LEFT TO RIGHT -- NO DANGER OF
C           OVERLAP WITH THE WORKING ELEMENTS IN L (PIVOTS). 
C     
          len_total = len_total + 1 
          WC(len_total) = fact
          IWC(len_total)  = jrow
          GOTO 150
 160      CONTINUE
C     
C-----------RESET DOUBLE-POINTER TO ZERO (U-PART)
          DO k = 1, len_upper
            IWC(NIAC+IWC(n+k-1)) = 0
          END DO
C     
C-----------UPDATE L-MATRIX
          len_lower = len_total 
          len_total = MIN0(len_lower,NLEVELS)
C     
C-----------SORT BY QUICK-SPLIT
          CALL SPKS_MILUT_QSPLIT(len_lower,len_total,WC,IWC)
C     
C-------------INSUFFICIENT STORAGE IN L.
            IF ( (iapc0 + len_total).GT.NNZAPC ) THEN
              WRITE (*,'(//1x,2i10)') iapc0, NNZAPC
              CALL USTOP('INSUFFICIENT STORAGE IN L')
            END IF
C
C-----------STORE L-PART
          DO k = 1, len_total
            APC(iapc0)  =  WC(k)
            JAPC(iapc0) =  IWC(k)
            iapc0 = iapc0 + 1
          END DO
C     
C-----------SAVE POINTER TO BEGINNING OF ROW n OF U
          JAPC(n) = iapc0
          IU(n) = iapc0
C
C-----------UPDATE U-MATRIX -- FIRST APPLY DROPPING STRATEGY 
          len_total = 0
          DO k = 1, len_upper-1
            IF ( DABS(WC(n+k)).GT.(DROPTOL*tnorm) ) THEN 
              len_total = len_total + 1
              WC(n+len_total)  = WC(n+k) 
              IWC(n+len_total) = IWC(n+k) 
            END IF
          END DO
          len_upper = len_total + 1
          len_total = MIN0(len_upper,NLEVELS)
C
          CALL SPKS_MILUT_QSPLIT( len_upper-1,len_total,
     2                            WC(n+1),IWC(n+1) )
C
C----------COPY
          t = DABS(WC(n))
C     
C-----------INSUFFICIENT STORAGE IN U.
          IF ( (len_total + iapc0).GT.NNZAPC ) THEN
            WRITE (*,'(//1x,2i10)') (len_total + iapc0), NNZAPC
            CALL USTOP('INSUFFICIENT STORAGE IN U')
          END IF
          DO k = (n+1), (n+len_total-1) 
            JAPC(iapc0) = IWC(k)
            APC(iapc0)  = WC(k)
            t = t + DABS( WC(k) )
            iapc0 = iapc0+1
          END DO
C
C-----------ROW SUM AGREEMENT
          rs  = DZERO
          DO k = (n+len_total), (n+len_upper-1)
            rs = rs + WC(k)
          END DO
C     
C-----------STORE INVERSE OF DIAGONAL ELEMENT OF U
          d   = WC(n)
          tl  = ( DONE + DELTA ) * d - ( DRELAX * rs )
C     
C-----------ENSURE THAT THE SIGN OF THE DIAGONAL HAS NOT CHANGED AND IS NOT ZERO
          sd1 = DSIGN(d,tl)
          IF ( sd1.NE.d ) THEN
C             USE SMALL VALUE IF DIAGONAL SCALING IS NOT EFFECTIVE FOR ELIMINATING
C             PIVOTS THAT CHANGE THE SIGN OF THE DIAGONAL
            IF ( IZERO.GT.1 ) THEN
              tl = DSIGN(DTINY,d)
C             DIAGONAL SCALING CONTINUES TO BE EFFECTIVE
            ELSE
              IZERO = 1
              EXIT MAINL
            END IF
          END IF
          IF ( ABS(tl).EQ.DZERO ) THEN
C             USE SMALL VALUE IF DIAGONAL SCALING IS NOT EFFECTIVE FOR ELIMINATING
C             ZERO PIVOTS
            IF ( IZERO.GT.1 ) THEN
              tl = DSIGN(DTINY,d)
C             DIAGONAL SCALING CONTINUES TO BE EFFECTIVE FOR ELIMIATING ZERO PIVOTS
            ELSE
              IZERO = 1
              EXIT MAINL
            END IF
          END IF
          APC(n) = DONE / tl
C     
C-----------UPDATE POINTER TO BEGINNING OF NEXT ROW OF U.
          IAPC(n+1) = iapc0
C-----------------------------------------------------------------------
C     END MAIN LOOP
C-----------------------------------------------------------------------
        END DO MAINL
        IF ( IZERO.GT.1 ) IZERO = 0
        DEALLOCATE(iu)
C---------RETURN
        RETURN
      END SUBROUTINE SPKS_MILUT

      SUBROUTINE SPKS_MILUT_QSPLIT(N,NCUT,A,IND)
C-----------------------------------------------------------------------
C     DOES A QUICK-SORT SPLIT OF A REAL ARRAY.
C     ON INPUT A(1:N). IS A REAL ARRAY
C     ON OUTPUT A(1:N) IS PERMUTED SUCH THAT ITS ELEMENTS SATISFY:
C
C     ABS(A(I)) .GE. ABS(A(NCUT)) FOR I .LT. NCUT AND
C     ABS(A(I)) .LE. ABS(A(NCUT)) FOR I .GT. NCUT
C
C     IND(1:N) IS AN INTEGER ARRAY WHICH PERMUTED IN THE SAME WAY AS A(*).
C-----------------------------------------------------------------------
C        SPECIFICATIONS:
C-----------------------------------------------------------------------
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: N
        INTEGER, INTENT(IN) :: NCUT
        DOUBLEPRECISION, DIMENSION(N), INTENT(INOUT) :: A
        INTEGER, DIMENSION(N), INTENT(INOUT) :: IND
C       + + + LOCAL DEFINITIONS + + +
        INTEGER :: j
        INTEGER :: itmp, first, mid, last
        DOUBLEPRECISION :: tmp, abskey
C       + + + FUNCTIONS + + +
C       + + + FORMATS + + +
C       + + + DATA + + +
C       + + + CODE + + +
        first = 1
        last = N
        IF ( NCUT.LT.first .OR. NCUT.GT.last ) RETURN
C
C---------OUTER LOOP -- WHILE mid.NE NCUT DO
        OUTERL: DO
          mid = first
          abskey = ABS(A(mid))
          DO j = first+1, last
            IF ( ABS(A(j)).GT.abskey ) then
              mid = mid+1
C
C---------------INTERCHANGE
              tmp = A(mid)
              itmp = IND(mid)
              A(mid) = A(j)
              IND(mid) = IND(j)
              A(j)  = tmp
              IND(j) = itmp
            END IF
          END DO
C
C-----------INTERCHANGE
          tmp = A(mid)
          A(mid) = A(first)
          A(first)  = tmp
C
          itmp = IND(mid)
          IND(mid) = IND(first)
          IND(first) = itmp
C
C-----------TEST FOR WHILE LOOP
          IF ( mid.EQ.NCUT ) EXIT OUTERL
          IF ( mid.GT.NCUT ) THEN
           last = mid-1
          ELSE
           first = mid+1
          END IF
        END DO OUTERL
C---------RETURN     
        RETURN
      END SUBROUTINE SPKS_MILUT_QSPLIT
C
C-------MATRIX AND VECTOR ROUTINES
C       MATRIX VECTOR PRODUCT
      SUBROUTINE SPKS_CMATVEC(NCORESM,NR,NNZ,IA,JA,A,V1,V2)      
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NCORESM
        INTEGER, INTENT(IN) :: NR
        INTEGER, INTENT(IN) :: NNZ
        INTEGER, DIMENSION(NR+1), INTENT(IN) :: IA
        INTEGER, DIMENSION(NNZ), INTENT(IN)  :: JA
        DOUBLEPRECISION, DIMENSION(NNZ),  INTENT(IN)    :: A
        DOUBLEPRECISION, DIMENSION(NR),   INTENT(IN)    :: V1
        DOUBLEPRECISION, DIMENSION(NR),   INTENT(INOUT) :: V2
C     + + + LOCAL DEFINITIONS + + +
        DOUBLEPRECISION, PARAMETER :: DZERO = 0.0D0
        DOUBLEPRECISION, PARAMETER :: DONE  = 1.0D0
        DOUBLEPRECISION :: sum
        INTEGER :: ic0, ic1
        INTEGER :: icol
        INTEGER :: m, n
        INTEGER :: n0, iblksize
        INTEGER :: i
        INTEGER :: istart, iend
        INTEGER :: jstart, jend
        INTEGER :: jlen
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        IF ( NCORESM.NE.1 ) THEN
          n0 = ( ( NR - 1 ) / NCORESM ) + 1
!$OMP  PARALLEL 
!$OMP& SHARED(n0,NR,A,V1,V2)
!$OMP& PRIVATE(iblksize,istart,iend,jstart,jend,jlen)
!$OMP& NUM_THREADS(NCORESM)
!$OMP  DO SCHEDULE(STATIC) 
          DO i = 1, NCORESM
            iblksize = MIN( n0, NR - ( i - 1 ) * n0 )
            istart   = ( i - 1 ) * n0 + 1
            iend     = istart + iblksize
            jstart   = IA(istart)
            jend     = IA(iend)
            jlen     = jend - jstart + 1
            IF ( iblksize.GT.0 ) THEN 
              CALL SPKS_GEMV(iblksize,NR,jlen,jstart,
     2                       IA(istart),JA(jstart),
     3                       A(jstart),V1(1),V2(istart))
            END IF
          END DO
!$OMP  END DO NOWAIT
!$OMP  END PARALLEL
        ELSE
          DO n = 1, NR
            sum = DZERO
C             ADD DIAGONAL AND OFF-DIAGONAL TERMS
            ic0 = IA(n)
            ic1 = IA(n+1) - 1
            DO m = ic0, ic1
              icol = JA(m) 
              sum = sum + A(m) * V1(icol)
            END DO
            V2(n) = sum
          END DO
        END IF
C---------RETURN
        RETURN
      END SUBROUTINE SPKS_CMATVEC

      SUBROUTINE SPKS_GEMV(IBLKSIZE,NR,JLEN,JSTART,IA,JA,A,V1,V2)
        IMPLICIT NONE
C         + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: IBLKSIZE
        INTEGER, INTENT(IN) :: NR
        INTEGER, INTENT(IN) :: JLEN
        INTEGER, INTENT(IN) :: JSTART
        INTEGER, DIMENSION(IBLKSIZE+1),       INTENT(IN)    :: IA
        INTEGER, DIMENSION(JLEN),             INTENT(IN)    :: JA
        DOUBLEPRECISION, DIMENSION(JLEN),     INTENT(IN)    :: A
        DOUBLEPRECISION, DIMENSION(NR),       INTENT(IN)    :: V1
        DOUBLEPRECISION, DIMENSION(IBLKSIZE), INTENT(INOUT) :: V2
C         + + + LOCAL DEFINITIONS + + +
        INTEGER :: i, j
        INTEGER :: j0, j1, jcol
        DOUBLEPRECISION, PARAMETER :: dzero = 0.0d0
C         + + + FUNCTIONS + + +
C         + + + CODE + + +
        DO i = 1, IBLKSIZE
          j0 = IA(i)   - JSTART + 1
          j1 = IA(i+1) - JSTART
          V2(i) = dzero
          DO j = j0, j1
            jcol = JA(j)
            V2(i) = V2(i) + A(j) * V1(jcol)
          END DO
        END DO
C---------RETURN
        RETURN
      END SUBROUTINE SPKS_GEMV
      
C       ADD PRODUCT OF CONSTANT AND VECTOR TO VECTOR
      SUBROUTINE SPKS_AXPY(NCORESV, NR, V1, C, V2, R)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NCORESV
        INTEGER, INTENT(IN) :: NR
        DOUBLEPRECISION, DIMENSION(NR), INTENT(IN)    :: V1
        DOUBLEPRECISION, INTENT(IN) :: C
        DOUBLEPRECISION, DIMENSION(NR), INTENT(IN)    :: V2
        DOUBLEPRECISION, DIMENSION(NR), INTENT(INOUT) :: R
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
        INTEGER :: n0
        INTEGER :: i
        INTEGER :: iblksize, istart
        DOUBLEPRECISION, PARAMETER :: DZERO = 0.0D0
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        IF ( NCORESV.NE.1 ) THEN
          n0 = ( ( NR - 1 ) / NCORESV ) + 1
!$OMP  PARALLEL 
!$OMP& SHARED(n0,NR,V1,C,V2,R)
!$OMP& PRIVATE(iblksize,istart)
!$OMP& NUM_THREADS(NCORESV)
!$OMP  DO SCHEDULE(STATIC) 
          DO i = 1, NCORESV
            iblksize = MIN( n0, NR - ( i - 1 ) * n0 )
            istart   = ( i - 1 ) * n0 + 1
            !iend     = istart + iblksize
            IF ( iblksize.GT.0 ) THEN 
              CALL SPKS_MCAXPY(iblksize,V1(istart),
     2                         C,V2(istart),R(istart))
            END IF
          END DO
!$OMP  END DO NOWAIT
!$OMP  END PARALLEL
        ELSE
          DO n = 1, NR
            R(n) = V1(n) + C * V2(n)
          END DO
        END IF
C---------RETURN
        RETURN
      END SUBROUTINE SPKS_AXPY

      SUBROUTINE SPKS_MCAXPY(IBLKSIZE, V1, C, V2, R)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: IBLKSIZE
        DOUBLEPRECISION, DIMENSION(IBLKSIZE), INTENT(IN)    :: V1
        DOUBLEPRECISION, INTENT(IN) :: C
        DOUBLEPRECISION, DIMENSION(IBLKSIZE), INTENT(IN)    :: V2
        DOUBLEPRECISION, DIMENSION(IBLKSIZE), INTENT(INOUT) :: R
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
         DO n = 1, IBLKSIZE
           R(n) = V1(n) + C * V2(n)
         END DO
C---------RETURN
        RETURN
      END SUBROUTINE SPKS_MCAXPY

C       RETURN THE ELEMENT BY ELEMENT PRODUCT OF TWO VECTORS
      SUBROUTINE SPKS_VVMULT(NCORESV, NR, V1, V2, R)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NCORESV
        INTEGER, INTENT(IN) :: NR
        DOUBLEPRECISION, DIMENSION(NR), INTENT(IN)    :: V1
        DOUBLEPRECISION, DIMENSION(NR), INTENT(IN)    :: V2
        DOUBLEPRECISION, DIMENSION(NR), INTENT(INOUT) :: R
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
        INTEGER :: n0
        INTEGER :: i
        INTEGER :: iblksize, istart
        DOUBLEPRECISION, PARAMETER :: DZERO = 0.0D0
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        IF ( NCORESV.NE.1 ) THEN
          n0 = ( ( NR - 1 ) / NCORESV ) + 1
!$OMP  PARALLEL 
!$OMP& SHARED(n0,NR,V1,V2,R)
!$OMP& PRIVATE(iblksize,istart)
!$OMP& NUM_THREADS(NCORESV)
!$OMP  DO SCHEDULE(STATIC) 
          DO i = 1, NCORESV
            iblksize = MIN( n0, NR - ( i - 1 ) * n0 )
            istart   = ( i - 1 ) * n0 + 1
            !iend     = istart + iblksize
            IF ( iblksize.GT.0 ) THEN 
              CALL SPKS_MCVVMULT(iblksize,V1(istart),
     2                           V2(istart),R(istart))
            END IF
          END DO
!$OMP  END DO NOWAIT
!$OMP  END PARALLEL
        ELSE
          DO n = 1, NR
            R(n) = V1(n) * V2(n)
          END DO
        END IF
C---------RETURN
        RETURN
      END SUBROUTINE SPKS_VVMULT

      SUBROUTINE SPKS_MCVVMULT(IBLKSIZE, V1, V2, R)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: IBLKSIZE
        DOUBLEPRECISION, DIMENSION(IBLKSIZE), INTENT(IN)    :: V1
        DOUBLEPRECISION, DIMENSION(IBLKSIZE), INTENT(IN)    :: V2
        DOUBLEPRECISION, DIMENSION(IBLKSIZE), INTENT(INOUT) :: R
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
         DO n = 1, IBLKSIZE
          R(n) = V1(n) * V2(n)
         END DO
C---------RETURN
        RETURN
      END SUBROUTINE SPKS_MCVVMULT
C
C       SET DOUBLE VECTOR TO A CONSTANT
      SUBROUTINE SPKS_DSET(NCORESV, NR, C, R)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NCORESV
        INTEGER, INTENT(IN) :: NR
        DOUBLEPRECISION, INTENT(IN) :: C
        DOUBLEPRECISION, DIMENSION(NR), INTENT(INOUT) :: R
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: i, n
        INTEGER :: iblksize, istart
        INTEGER :: n0
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        IF ( NCORESV.NE.1 ) THEN
          n0 = ( ( NR - 1 ) / NCORESV ) + 1
!$OMP  PARALLEL
!$OMP& NUM_THREADS(NCORESV)
!$OMP& SHARED(n0,NR,C,R)
!$OMP& PRIVATE(iblksize,istart)
!$OMP  DO SCHEDULE(STATIC)
          DO i = 1, NCORESV
            iblksize = MIN( n0, NR - ( i - 1 ) * n0 )
            istart   = ( i - 1 ) * n0 + 1
            IF ( iblksize.GT.0 ) THEN 
              CALL SPKS_MCDSET(iblksize,C,R(istart))
            END IF
          END DO
!$OMP  END DO NOWAIT
!$OMP  END PARALLEL
        ELSE           
          DO n = 1, NR
            R(n) = C
          END DO
        END IF
C---------RETURN
        RETURN
      END SUBROUTINE SPKS_DSET

      SUBROUTINE SPKS_MCDSET(IBLKSIZE, C, R)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) ::IBLKSIZE
        DOUBLEPRECISION, INTENT(IN) :: C
        DOUBLEPRECISION, DIMENSION(IBLKSIZE), INTENT(INOUT) :: R
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        DO n = 1, IBLKSIZE
          R(n) = C
        END DO
C---------RETURN
        RETURN
      END SUBROUTINE SPKS_MCDSET
C
C       MULTIPLY A DOUBLE VECTOR BY A CONSTANT
      SUBROUTINE SPKS_DSCAL(NCORESV, NR, C, R)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NCORESV
        INTEGER, INTENT(IN) :: NR
        DOUBLEPRECISION, INTENT(IN) :: C
        DOUBLEPRECISION, DIMENSION(NR), INTENT(INOUT) :: R
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: i, n
        INTEGER :: iblksize, istart
        INTEGER :: n0
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        IF ( NCORESV.NE.1 ) THEN
          n0 = ( ( NR - 1 ) / NCORESV ) + 1
!$OMP  PARALLEL
!$OMP& NUM_THREADS(NCORESV)
!$OMP& SHARED(n0,NR,C,R)
!$OMP& PRIVATE(iblksize,istart)
!$OMP  DO SCHEDULE(STATIC)
          DO i = 1, NCORESV
            iblksize = MIN( n0, NR - ( i - 1 ) * n0 )
            istart   = ( i - 1 ) * n0 + 1
            IF ( iblksize.GT.0 ) THEN 
              CALL SPKS_MCDSCAL(iblksize,C,R(istart))
            END IF
          END DO
!$OMP  END DO NOWAIT
!$OMP  END PARALLEL
        ELSE           
          DO n = 1, NR
            R(n) = R(n) * C
          END DO
        END IF
C---------RETURN
        RETURN
      END SUBROUTINE SPKS_DSCAL

      SUBROUTINE SPKS_MCDSCAL(IBLKSIZE, C, R)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) ::IBLKSIZE
        DOUBLEPRECISION, INTENT(IN) :: C
        DOUBLEPRECISION, DIMENSION(IBLKSIZE), INTENT(INOUT) :: R
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        DO n = 1, IBLKSIZE
          R(n) = R(n) * C
        END DO
C---------RETURN
        RETURN
      END SUBROUTINE SPKS_MCDSCAL
C
C       SET INTEGER VECTOR TO A CONSTANT
      SUBROUTINE SPKS_ISET(NCORESV, NR, IC, IV)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NCORESV
        INTEGER, INTENT(IN) :: NR
        INTEGER, INTENT(IN) :: IC
        INTEGER, DIMENSION(NR), INTENT(INOUT) :: IV
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: i, n
        INTEGER :: iblksize, istart
        INTEGER :: n0
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        IF ( NCORESV.NE.1 ) THEN
          n0 = ( ( NR - 1 ) / NCORESV ) + 1
!$OMP  PARALLEL
!$OMP& NUM_THREADS(NCORESV)
!$OMP& SHARED(n0,NR,IC,IV)
!$OMP& PRIVATE(iblksize,istart)
!$OMP  DO SCHEDULE(STATIC)
          DO i = 1, NCORESV
            iblksize = MIN( n0, NR - ( i - 1 ) * n0 )
            istart   = ( i - 1 ) * n0 + 1
            IF ( iblksize.GT.0 ) THEN 
              CALL SPKS_MCISET(iblksize,IC,IV(istart))
            END IF
          END DO
!$OMP  END DO NOWAIT
!$OMP  END PARALLEL
        ELSE
          DO n = 1, NR
            IV(n) = IC
          END DO
        END IF
C---------RETURN
        RETURN
      END SUBROUTINE SPKS_ISET

      SUBROUTINE SPKS_MCISET(IBLKSIZE, IC, IV)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) ::IBLKSIZE
        INTEGER, INTENT(IN) :: IC
        INTEGER, DIMENSION(IBLKSIZE), INTENT(INOUT) :: IV
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        DO n = 1, IBLKSIZE
          IV(n) = IC
        END DO
C---------RETURN
        RETURN
      END SUBROUTINE SPKS_MCISET
C
C       COPY ONE INTEGER VECTOR TO ANOTHER
      SUBROUTINE SPKS_ICOPY(NCORESV, NR, IV, IR)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NCORESV
        INTEGER, INTENT(IN) :: NR
        INTEGER, DIMENSION(NR), INTENT(IN)    :: IV
        INTEGER, DIMENSION(NR), INTENT(INOUT) :: IR
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: i, n
        INTEGER :: iblksize, istart
        INTEGER :: n0
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        IF ( NCORESV.NE.1 ) THEN
          n0 = ( ( NR - 1 ) / NCORESV ) + 1
!$OMP  PARALLEL
!$OMP& NUM_THREADS(NCORESV)
!$OMP& SHARED(n0,NR,IV,IR)
!$OMP& PRIVATE(iblksize,istart)
!$OMP  DO SCHEDULE(STATIC)
          DO i = 1, NCORESV
            iblksize = MIN( n0, NR - ( i - 1 ) * n0 )
            istart   = ( i - 1 ) * n0 + 1
            IF ( iblksize.GT.0 ) THEN 
              CALL SPKS_MCICOPY(iblksize,IV(istart),IR(istart))
            END IF
          END DO
!$OMP  END DO NOWAIT
!$OMP  END PARALLEL
        ELSE
          DO n = 1, NR
            IR(n) = IV(n)
          END DO
        END IF
C---------RETURN
        RETURN
      END SUBROUTINE SPKS_ICOPY

      SUBROUTINE SPKS_MCICOPY(IBLKSIZE, IV, IR)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) ::IBLKSIZE
        INTEGER, DIMENSION(IBLKSIZE), INTENT(IN)    :: IV
        INTEGER, DIMENSION(IBLKSIZE), INTENT(INOUT) :: IR
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        DO n = 1, IBLKSIZE
          IR(n) = IV(n)
        END DO
C---------RETURN
        RETURN
      END SUBROUTINE SPKS_MCICOPY

C       COPY ONE DOUBLEPRECISION VECTOR TO ANOTHER
      SUBROUTINE SPKS_DCOPY(NCORESV, NR, V, R)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NCORESV
        INTEGER, INTENT(IN) :: NR
        DOUBLEPRECISION, DIMENSION(NR), INTENT(IN)    :: V
        DOUBLEPRECISION, DIMENSION(NR), INTENT(INOUT) :: R
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: i, n
        INTEGER :: iblksize, istart
        INTEGER :: n0
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        IF ( NCORESV.NE.1 ) THEN
          n0 = ( ( NR - 1 ) / NCORESV ) + 1
!$OMP  PARALLEL
!$OMP& NUM_THREADS(NCORESV)
!$OMP& SHARED(n0,NR,V,R)
!$OMP& PRIVATE(iblksize,istart)
!$OMP  DO SCHEDULE(STATIC)
          DO i = 1, NCORESV
            iblksize = MIN( n0, NR - ( i - 1 ) * n0 )
            istart   = ( i - 1 ) * n0 + 1
            IF ( iblksize.GT.0 ) THEN 
              CALL SPKS_MCDCOPY(iblksize,V(istart),R(istart))
            END IF
          END DO
!$OMP  END DO NOWAIT
!$OMP  END PARALLEL
        ELSE
          DO n = 1, NR
            R(n) = V(n)
          END DO
        END IF
C---------RETURN
        RETURN
      END SUBROUTINE SPKS_DCOPY

      SUBROUTINE SPKS_MCDCOPY(IBLKSIZE, V, R)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) ::IBLKSIZE
        DOUBLEPRECISION, DIMENSION(IBLKSIZE), INTENT(IN)    :: V
        DOUBLEPRECISION, DIMENSION(IBLKSIZE), INTENT(INOUT) :: R
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        DO n = 1, IBLKSIZE
          R(n) = V(n)
        END DO
C---------RETURN
        RETURN
      END SUBROUTINE SPKS_MCDCOPY
C
C-------SIMPLE INPLACE SORTING ROUTINE FOR AN INTEGER ARRAY      
      SUBROUTINE SPKS_ISORT(NVAL,IARRAY)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER,INTENT(IN) :: NVAL
        INTEGER,DIMENSION(NVAL),INTENT(INOUT) :: IARRAY
C     + + + LOCAL DEFINITIONS + + +
        integer :: i, j, itemp
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        DO i = 1, NVAL-1
            DO j = i+1, NVAL
                if(IARRAY(i).GT.IARRAY(j)) then
                    itemp = IARRAY(j)
                    IARRAY(j) = IARRAY(i)
                    IARRAY(i) = itemp
                END IF
            END DO
        END DO
      END SUBROUTINE SPKS_ISORT
      
      DOUBLEPRECISION FUNCTION SPKS_DOTP(NCORESV,NRPROC,NR,V1,V2,IXMAP)
     1   RESULT(C)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NCORESV
        INTEGER, INTENT(IN) :: NRPROC                                   !JV
        INTEGER, INTENT(IN) :: NR
        DOUBLEPRECISION, DIMENSION(NR),  INTENT(IN)    :: V1
        DOUBLEPRECISION, DIMENSION(NR),  INTENT(IN)    :: V2
        INTEGER, DIMENSION(NR),  INTENT(IN)            :: IXMAP         !JV
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n0
        INTEGER :: i, iblksize, istart
        DOUBLEPRECISION, PARAMETER :: DZERO = 0.0D0
C     + + + FUNCTIONS + + +
        DOUBLEPRECISION :: SPKS_MCDOTP
        DOUBLEPRECISION :: SPKS_DOTP_MPP                                !JV
C     + + + CODE + + +
C
        C = DZERO
        IF ( NCORESV.NE.1 ) THEN
          n0 = ( ( NR - 1 ) / NCORESV ) + 1
!$OMP  PARALLEL
!$OMP& NUM_THREADS(NCORESV)
!$OMP& SHARED(n0,NR,V1,V2,NRPROC,IXMAP)
!$OMP& PRIVATE(iblksize,istart)
!$OMP& REDUCTION(+: C)
!$OMP  DO SCHEDULE(STATIC)
          DO i = 1, NCORESV
            iblksize = MIN( n0, NR - ( i - 1 ) * n0 )
            istart   = ( i - 1 ) * n0 + 1
            IF ( iblksize.GT.0 ) THEN
              C = SPKS_MCDOTP(iblksize, V1(istart), V2(istart), 
     1           NRPROC,IXMAP(ISTART))                                  !JV
            END IF
          END DO
!$OMP  END DO NOWAIT
!$OMP  END PARALLEL
        ELSEIF(NRPROC.EQ.1) THEN
          C = DOT_PRODUCT(V1, V2)
        ELSE
          C = SPKS_DOTP_MPP(NR, V1, V2, IXMAP)                          !JV
        END IF
C---------RETURN
        RETURN
      END FUNCTION SPKS_DOTP

      DOUBLEPRECISION FUNCTION SPKS_MCDOTP(IBLKSIZE,V1,V2,NRPROC,IXMAP)
     1  RESULT(R)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: IBLKSIZE
        DOUBLEPRECISION, DIMENSION(IBLKSIZE), INTENT(IN) :: V1
        DOUBLEPRECISION, DIMENSION(IBLKSIZE), INTENT(IN) :: V2
        INTEGER, INTENT(IN) :: NRPROC                                   !JV
        INTEGER, DIMENSION(IBLKSIZE), INTENT(IN) :: IXMAP
C     + + + LOCAL DEFINITIONS + + +
C     + + + FUNCTIONS + + + 
        DOUBLEPRECISION DOT_PRODUCT_PKS
        DOUBLEPRECISION SPKS_DOTP_MPP                                   !JV
C     + + + CODE + + +
        IF (NRPROC.EQ.1) THEN                                           !JV
          R = DOT_PRODUCT(V1, V2)
        ELSE                                                            !JV
          R = SPKS_DOTP_MPP(IBLKSIZE, V1, V2, IXMAP)                    !JV
        ENDIF                                                           !JV
C---------RETURN
        RETURN
      END FUNCTION SPKS_MCDOTP
C
C-------CALCULATE THE L2 NORM OF A VECTOR
      DOUBLEPRECISION FUNCTION SPKS_L2NORM(NCORESV,NRPROC,NR,V,IXMAP)
     1  RESULT(value)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NCORESV
        INTEGER, INTENT(IN) :: NRPROC                                   !JV
        INTEGER, INTENT(IN) :: NR
        DOUBLEPRECISION, DIMENSION(NR), INTENT(IN) :: V
        INTEGER, DIMENSION(NR), INTENT(IN) :: IXMAP                     !JV
C     + + + LOCAL DEFINITIONS + + +
        DOUBLEPRECISION :: dotp
C     + + + FUNCTIONS + + +
        DOUBLEPRECISION :: SPKS_DOTP
C     + + + CODE + + +
        dotp = SPKS_DOTP(NCORESV,NRPROC,NR,V,V,IXMAP)                   !JV
        IF (NRPROC.GT.1) THEN                                           !JV
           value = dotp             
        ELSE
           value = SQRT(dotp)
        END IF         
C---------RETURN
        RETURN
      END FUNCTION SPKS_L2NORM

      INTEGER FUNCTION SPKS_LOCMAX(NCORESV,NR,V1,IXMAP) RESULT(IVAL)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NCORESV
        INTEGER, INTENT(IN) :: NR
        DOUBLEPRECISION, DIMENSION(NR),  INTENT(IN)    :: V1
        INTEGER, DIMENSION(NR),  INTENT(IN)            :: IXMAP         !JV
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
        INTEGER :: n0
        DOUBLEPRECISION :: c
        DOUBLEPRECISION :: t
        INTEGER :: i, iblksize, istart
        DOUBLEPRECISION, PARAMETER :: DZERO = 0.0D0
C     + + + FUNCTIONS + + +
        DOUBLEPRECISION :: SPKS_MCINFNORM
        INTEGER :: SPKS_MCLOCMAX
C     + + + CODE + + +
C
        IVAL = 0
        c = DZERO
        IF ( NCORESV.NE.1 ) THEN
          n0 = ( ( NR - 1 ) / NCORESV ) + 1
!$OMP  PARALLEL
!$OMP& NUM_THREADS(NCORESV)
!$OMP& SHARED(n0,NR,V1)
!$OMP& PRIVATE(iblksize,istart)
!$OMP& REDUCTION(MAX: c)
!$OMP  DO SCHEDULE(STATIC)
          DO i = 1, NCORESV
            iblksize = MIN( n0, NR - ( i - 1 ) * n0 )
            istart   = ( i - 1 ) * n0 + 1
            IF ( iblksize.GT.0 ) THEN 
              C = SPKS_MCINFNORM(iblksize, V1(istart), ixmap(istart))   !JV
            END IF
          END DO
!$OMP  END DO NOWAIT
!$OMP  END PARALLEL
!$OMP  PARALLEL
!$OMP& NUM_THREADS(NCORESV)
!$OMP& SHARED(n0,NR,V1)
!$OMP& PRIVATE(iblksize,istart)
!$OMP& REDUCTION(MAX: ival)
!$OMP  DO SCHEDULE(STATIC)
          DO i = 1, NCORESV
            iblksize = MIN( n0, NR - ( i - 1 ) * n0 )
            istart   = ( i - 1 ) * n0 + 1
            IF ( iblksize.GT.0 ) THEN 
              IVAL = SPKS_MCLOCMAX(c,iblksize,V1(istart),IXMAP(istart)) !JV
            END IF
          END DO
!$OMP  END DO NOWAIT
!$OMP  END PARALLEL
        ELSE
          DO n = 1, NR
            t = ABS(V1(n))
            IF ( t.GE.C.AND.IXMAP(n).GT.0) THEN                         !JV
              c = t
!              IVAL = IXMAP(n)
              IVAL = n              
            END IF
          END DO
        END IF
C---------RETURN
        RETURN
      END FUNCTION SPKS_LOCMAX

      INTEGER FUNCTION SPKS_MCLOCMAX(VAL,IBLKSIZE,V1,IXMAP) RESULT(IVAL)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        DOUBLEPRECISION, INTENT(IN) :: VAL
        INTEGER, INTENT(IN) ::IBLKSIZE
        DOUBLEPRECISION, DIMENSION(IBLKSIZE), INTENT(IN) :: V1
        INTEGER, DIMENSION(IBLKSIZE), INTENT(IN) :: IXMAP
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
        DOUBLEPRECISION :: r
        DOUBLEPRECISION :: t
        DOUBLEPRECISION, PARAMETER :: DZERO = 0.0D0
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        IVAL = 0
        r = DZERO
        DO n = 1, IBLKSIZE
          t = ABS(V1(n))
          IF ( t.EQ.val .AND. IXMAP(n).GT.0) THEN                       !JV
!            IVAL = IXMAP(n)
             IVAL = n
          END IF
        END DO
C---------RETURN
        RETURN
      END FUNCTION SPKS_MCLOCMAX

      DOUBLEPRECISION FUNCTION SPKS_INFNORM(NCORESV,NR,V1,IXMAP)        !JV
     &  RESULT(C)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NCORESV
        INTEGER, INTENT(IN) :: NR
        DOUBLEPRECISION, DIMENSION(NR),  INTENT(IN)    :: V1
        INTEGER, DIMENSION(NR),  INTENT(IN)            :: IXMAP         !JV
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
        INTEGER :: n0
        DOUBLEPRECISION :: t
        INTEGER :: i, iblksize, istart
        DOUBLEPRECISION, PARAMETER :: DZERO = 0.0D0
C     + + + FUNCTIONS + + +
        DOUBLEPRECISION :: SPKS_MCINFNORM
C     + + + CODE + + +
C
        C = DZERO
        IF ( NCORESV.NE.1 ) THEN
          n0 = ( ( NR - 1 ) / NCORESV ) + 1
!$OMP  PARALLEL
!$OMP& NUM_THREADS(NCORESV)
!$OMP& SHARED(n0,NR,V1)
!$OMP& PRIVATE(iblksize,istart)
!$OMP& REDUCTION(MAX: C)
!$OMP  DO SCHEDULE(STATIC)
          DO i = 1, NCORESV
            iblksize = MIN( n0, NR - ( i - 1 ) * n0 )
            istart   = ( i - 1 ) * n0 + 1
            IF ( iblksize.GT.0 ) THEN 
              C = SPKS_MCINFNORM(iblksize, V1(istart), ixmap(istart))   !JV
            END IF
          END DO
!$OMP  END DO NOWAIT
!$OMP  END PARALLEL
        ELSE
          DO n = 1, NR
            t = ABS(V1(n))
            IF ( t.GT.C.AND.IXMAP(N).GT.0) THEN                         !JV
              C = t
            END IF
          END DO
        END IF
C---------RETURN
        RETURN
      END FUNCTION SPKS_INFNORM

      DOUBLEPRECISION FUNCTION SPKS_MCINFNORM(IBLKSIZE,V1,MASK)         !JV
     &  RESULT(R)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) ::IBLKSIZE
        DOUBLEPRECISION, DIMENSION(IBLKSIZE), INTENT(IN) :: V1
        INTEGER, DIMENSION(IBLKSIZE), INTENT(IN) :: MASK                !JV
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
        DOUBLEPRECISION :: t
        DOUBLEPRECISION, PARAMETER :: DZERO = 0.0D0
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        R = DZERO
        DO n = 1, IBLKSIZE
          t = ABS(V1(n))
          IF ( t.GT.R.AND.MASK(N).GT.0) THEN                            !JV
            R = t
          END IF
          !R = MAX( R, ABS(V1(n)) )
        END DO
C---------RETURN
        RETURN
      END FUNCTION SPKS_MCINFNORM

C
C---------TEST FOR SOLVER CONVERGENCE
        SUBROUTINE SPKS_TESTCNVG( ICNVGOPT,KITER,MXITER,Iiter,
     &                            Iicnvg,ICNVG,
     &                            Rmax0,Epfact,Hmax,Rmax,Hclose,Rclose )
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: ICNVGOPT
        INTEGER, INTENT(IN) :: KITER
        INTEGER, INTENT(IN) :: MXITER
        INTEGER, INTENT(IN) :: Iiter
        INTEGER, INTENT(INOUT) ::Iicnvg
        INTEGER, INTENT(INOUT) ::ICNVG
        DOUBLEPRECISION, INTENT(IN) :: Rmax0
        DOUBLEPRECISION, INTENT(IN) :: Epfact
        DOUBLEPRECISION, INTENT(IN) :: Hmax
        DOUBLEPRECISION, INTENT(IN) :: Rmax
        DOUBLEPRECISION, INTENT(IN) :: Hclose
        DOUBLEPRECISION, INTENT(IN) :: Rclose
C     + + + LOCAL DEFINITIONS + + +
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        IF ( ICNVGOPT.EQ.0 ) THEN
          IF ( Hmax.LE.Hclose .AND. Rmax.LE.Rclose ) THEN
            Iicnvg = 1
          END IF
        ELSE IF ( ICNVGOPT.EQ.1 ) THEN
          IF ( Rmax.LE.Rclose ) THEN
            Iicnvg = 1
          ELSE IF ( Epfact.LT.1.0D0 ) THEN
          IF ( Rmax.LE.Rmax0*Epfact ) THEN
            Iicnvg = 1
          END IF
          END IF
        ELSE IF ( ICNVGOPT.EQ.2 ) THEN
          IF ( Hmax.LE.Hclose .AND. Rmax.LE.Rclose ) THEN
            Iicnvg = 1
          END IF
        END IF
        IF ( MXITER.EQ.1 ) THEN
          IF ( Iicnvg.EQ.1 ) ICNVG = 1
        ELSE
          IF ( ICNVGOPT.EQ.0 ) THEN
            IF ( Iiter.EQ.1 .AND. Iicnvg.EQ.1 .AND. 
     +           Hmax.LE.Hclose. AND. Rmax.LE.Rclose ) THEN
              ICNVG = 1
            END IF
          ELSE IF ( ICNVGOPT.EQ.1 ) THEN
            IF ( KITER.GT.1 .AND. Iicnvg.EQ.1 .AND.
     +           Hmax.LE.Hclose. AND. Rmax.LE.Rclose ) THEN
              ICNVG = 1
            END IF
          ELSE IF ( ICNVGOPT.EQ.2 ) THEN
            IF ( KITER.GT.1 .AND. Iicnvg.EQ.1 .AND.
     +           Hmax.LE.Hclose. AND. Rmax.LE.Rclose ) THEN
              ICNVG = 1
            END IF
          END IF
        ENDIF
C---------RETURN
        RETURN
      END SUBROUTINE SPKS_TESTCNVG
      
      SUBROUTINE SPKS_NODE2LRC(NLAY,NROW,NCOL,N,K,I,J)
C        SPECIFICATIONS:
C-----------------------------------------------------------------------
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN)    :: NLAY
        INTEGER, INTENT(IN)    :: NROW
        INTEGER, INTENT(IN)    :: NCOL
        INTEGER, INTENT(IN)    :: N
        INTEGER, INTENT(INOUT) :: K
        INTEGER, INTENT(INOUT) :: I
        INTEGER, INTENT(INOUT) :: J
C       + + + LOCAL DEFINITIONS + + +
        INTEGER :: nrc
        INTEGER :: ij
C       + + + FUNCTIONS + + +
C       + + + FORMATS + + +
C       + + + DATA + + +
C       + + + CODE + + +
        nrc = NROW * NCOL
        K  = N / nrc
        IF ( ( K * nrc ).LT.N ) THEN
          K = K + 1
        END IF 
        ij = N - ( K - 1 ) * nrc
        I  = ij / NCOL 
        if ( ( I * NCOL ).LT.ij ) THEN
          I = I + 1
        END IF
        J = ij - ( I - 1 ) * NCOL
C
C-------RETURN
      RETURN
C
      END SUBROUTINE SPKS_NODE2LRC
C
C-------TIMER FOR SWR CALCULATIONS
      SUBROUTINE SPKS_TIMER(It,T0,Dt)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: It
        REAL, INTENT(INOUT) :: T0
        REAL, INTENT(INOUT) :: Dt
C     + + + LOCAL DEFINITIONS + + +
        REAL :: t1
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        IF ( It.EQ.0 ) THEN
          T0 = SECNDS(0.0)
        ELSE
          t1 = SECNDS(T0)
          Dt = Dt + t1
        END IF
C---------RETURN
        RETURN
      END SUBROUTINE SPKS_TIMER

      DOUBLE PRECISION FUNCTION SPKS_DOTP_MPP(NR,V1,V2,MASK)           !JV
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: NR
        DOUBLE PRECISION, DIMENSION(NR), INTENT(IN) :: V1
        DOUBLE PRECISION, DIMENSION(NR), INTENT(IN) :: V2
        INTEGER, DIMENSION(NR), INTENT(IN) :: MASK

        INTEGER :: I
        DOUBLE PRECISION :: RES
  
        RES = 0.D0
        DO I = 1, NR
          IF (MASK(I).GT.0) RES = RES + V1(I)*V2(I)
        END DO
        SPKS_DOTP_MPP = RES

        RETURN 
      END FUNCTION SPKS_DOTP_MPP      
      
        SUBROUTINE SPKS_MILU0(NCORESV,NNZC,NIAC,NIWC,NWC,
     M                      AC,IAC,JAC,
     M                      APC,IAPC,JAPC,IWC,WC,
     M                      DRELAX,DELTA,IZERO,
     M                      IXMAP,NODES)                                !JV
        use pksmpi_mod, only: myrank, l2gnod
      
      
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NCORESV
        INTEGER, INTENT(IN) :: NNZC
        INTEGER, INTENT(IN) :: NIAC
        INTEGER, INTENT(IN) :: NIWC
        INTEGER, INTENT(IN) :: NWC
        DOUBLEPRECISION, DIMENSION(NNZC),  INTENT(IN)   :: AC
        INTEGER, DIMENSION(NIAC+1), INTENT(IN)   :: IAC
        INTEGER, DIMENSION(NNZC), INTENT(IN)     :: JAC
        DOUBLEPRECISION, DIMENSION(NNZC), INTENT(INOUT) :: APC
        INTEGER, DIMENSION(NIAC+1), INTENT(IN)   :: IAPC
        INTEGER, DIMENSION(NNZC), INTENT(IN)     :: JAPC
        INTEGER, DIMENSION(NIWC), INTENT(INOUT)  :: IWC
        DOUBLEPRECISION, DIMENSION(NWC), INTENT(INOUT) :: WC
        DOUBLEPRECISION, INTENT(IN) :: DRELAX
        DOUBLEPRECISION, INTENT(IN) :: DELTA
        INTEGER, INTENT(INOUT) :: IZERO
        INTEGER, DIMENSION(NIAC), INTENT(IN) :: IXMAP                   !JV
        INTEGER, INTENT(IN) :: NODES                                    !JV
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: ic0, ic1, id0, iu1
        INTEGER :: iu, iiu
        INTEGER :: iic0, iic1
        INTEGER :: j, n
        INTEGER :: jj
        INTEGER :: jpos, jcol, jw
        INTEGER :: jjcol
        DOUBLEPRECISION :: sd1
        DOUBLEPRECISION :: tl
        DOUBLEPRECISION :: rs
        DOUBLEPRECISION :: d
C     + + + PARAMETERS + + + 
        DOUBLEPRECISION, PARAMETER :: DZERO = 0.0D0
        DOUBLEPRECISION, PARAMETER :: DONE  = 1.0D0
        DOUBLEPRECISION, PARAMETER :: DTINY = 1.0D-06
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
C     
        CALL SPKS_DCOPY(NCORESV, NNZC, AC, APC)
C         CALCULATE PRECONDITIONER        
        CALL SPKS_ISET(NCORESV, NIAC, 0, IWC)
        CALL SPKS_DSET(NCORESV, NIAC, DZERO, WC)
C         SET COEFFICIENTS ARTIFICIAL BOUNDARY NODES        
        DO n = 1, NIAC                                                  ! JV
          IF ( IXMAP(n).LT.-NODES ) THEN                                ! JV
             APC(IAC(n)) = 1.0E20                                       ! JV
          END IF                                                        ! JV
        END DO                                                          ! JV
        MAIN: DO n = 1, NIAC
          ic0 = IAC(n)
          ic1 = IAC(n+1) - 1
          DO j = ic0, ic1
            jcol      = JAC(j)
            IWC(jcol) = 1
c            WC(jcol) = WC(jcol) + AC(j)
            IF ( IXMAP(n).LT.-NODES.and.j.eq.ic0) THEN                  ! JV
               WC(jcol) = WC(jcol) + 1.0E20                             ! JV
            ELSE                                                        ! JV
               WC(jcol) = WC(jcol) + AC(j)                              ! JV
            END IF                                                      ! JV
          END DO
          ic0 = IAPC(n)
          ic1 = IAPC(n+1) - 1
          iu  = JAPC(n)
          rs  = DZERO
          LOWER: DO j = ic0, iu-1
            jcol     = JAPC(j)
            iic0     = IAPC(jcol) 
            iic1     = IAPC(jcol+1) - 1
            iiu      = JAPC(jcol)
            tl       = WC(jcol) * APC(jcol)
            WC(jcol) = tl
            DO jj = iiu, iic1
              jjcol = JAPC(jj)
              jw    = IWC(jjcol)
              IF ( jw.NE.0 ) THEN
                WC(jjcol) = WC(jjcol) - tl * APC(jj)
              ELSE
                rs = rs + tl * APC(jj)
              END IF
            END DO
          END DO LOWER
C           DIAGONAL - CALCULATE INVERSE OF DIAGONAL FOR SOLUTION
          d   = WC(n)
          tl  = ( DONE + DELTA ) * d - ( DRELAX * rs )
C-----------ENSURE THAT THE SIGN OF THE DIAGONAL HAS NOT CHANGED AND IS NOT ZERO
          sd1 = DSIGN(d,tl)
          IF ( sd1.NE.d ) THEN
C             USE SMALL VALUE IF DIAGONAL SCALING IS NOT EFFECTIVE FOR ELIMINATING
C             PIVOTS THAT CHANGE THE SIGN OF THE DIAGONAL
            IF ( IZERO.GT.1 ) THEN
              tl = DSIGN(DTINY,d)
C             DIAGONAL SCALING CONTINUES TO BE EFFECTIVE
            ELSE
              IZERO = 1
              EXIT MAIN
            END IF
          END IF
          IF ( ABS(tl).EQ.DZERO ) THEN
C             USE SMALL VALUE IF DIAGONAL SCALING IS NOT EFFECTIVE FOR ELIMINATING
C             ZERO PIVOTS
            IF ( IZERO.GT.1 ) THEN
              tl = DSIGN(DTINY,d)
C             DIAGONAL SCALING CONTINUES TO BE EFFECTIVE FOR ELIMIATING ZERO PIVOTS
            ELSE
              IZERO = 1
              EXIT MAIN
            END IF
          END IF
          APC(n) = DONE / tl
C           RESET POINTER FOR IW TO ZERO
          IWC(n) = 0
          WC(n)  = DZERO
          DO j = ic0, ic1
            jcol = JAPC(j)
            APC(j) = WC(jcol)
            IWC(jcol) = 0
            WC(jcol) = DZERO
          END DO
        END DO MAIN
        IF ( IZERO.GT.1 ) IZERO = 0
C---------RETURN
        RETURN
      END SUBROUTINE SPKS_MILU0