!*------------------------------------------------------------------------------
!* FILE:
!*    IMOD_ISG_IFVDL.F90            
!*  
!* COPYRIGHT: 2007
!*    Alterra
!* 
!*    This PROGRAM, or parts thereof, may not be reproduced,
!*    modified or transferred to third parties without the
!*    written permission of Alterra
!* 
!*------------------------------------------------------------------------------
    
!###==================================================================== 
 REAL FUNCTION LEKCONDUCTANCE_Y (A,H0,KV,KH,C1,LI,BIN,C0)
 !###====================================================================
 !-----------------------------------------------------------------------
 !* DESCRIPTION:
 !*    Calculates de lekconductance according to De Lange
 !*    A = celoppervlak (m2)
 !*    H0 = doorstroomde dikte (m)
 !*    kv = verticale doorlatendheid (m/d)
 !*    kh = horizontale doorlatendheid (m/d)
 !*    c1 = deklaagweerstand (d)
 !*    li = lengte van de waterlopen (m)
 !*    Bin = Bodembreedte (m)
 !*    c0 = slootbodemweerstand (d)
 !*    Uitkomst: Lekconductance (m2/d)
 !*
 !*    Conductances are easier to handle than resistances
 !*    NB: L may not exceed SQRT(A), Lrad may because it is fair to assume that the total radial resistance is concentrated within A
 !*
 !-----------------------------------------------------------------------
 !
 IMPLICIT NONE
 REAL,INTENT(IN) :: A,H0,KV,KH,C1,LI,BIN,C0
 REAL :: L,BCOR,LABDAL,FL,LABDAB,FB,CL,CB,CRAD,WP,ACOR,PSL
 REAL :: X,Y,PI,CTNH

 PI = 4.0 * ATAN(1.0)
!
 IF (LI.GT.0.001 .AND. BIN .GT. 0.001 .AND. A.GT.0.001) THEN
  ACOR = A
  BCOR = MAX(BIN,0.001)
  L = A/LI-BCOR
  Y=C1+H0/KV
  LABDAL=SQRT(Y*KH*H0)
  X=0.5*L/LABDAL
  FL=X*CTNH(X)
  LABDAB=SQRT(Y*KH*H0*C0/(Y+C0))
  X=0.5*BCOR/LABDAB
  FB=X*CTNH(X)
  CL=(C0+Y)*FL+(C0*L/BCOR)*FB
  CB=(C1+C0+H0/KV)/(CL-C0*L/BCOR)*CL
  CRAD=MAX(0.,L/(PI*SQRT(KV*KH))*LOG(4*H0/(PI*BCOR)))
  PSL=BCOR*LI/A
  WP = 1/((1.-PSL)/CL+PSL/CB)+CRAD-Y
  LEKCONDUCTANCE_Y = A/WP
 ELSE
  LEKCONDUCTANCE_Y = 0.0
 ENDIF

 END FUNCTION LEKCONDUCTANCE_Y

 !###====================================================================
 REAL FUNCTION CTNH (X)
 !###====================================================================
 !-----------------------------------------------------------------------
 !* DESCRIPTION:
 !*    Calculates the cotangens hyperbolicus
 !*
 !-----------------------------------------------------------------------
 IMPLICIT NONE
 REAL :: x

 IF (X.LT.-4.0)THEN
  CTNH=-1.0
 ELSEIF(X.GT.4.0)THEN
  CTNH=1.0
 ELSE
  CTNH=(EXP(X)+EXP(-1*X))/(EXP(X)-EXP(-1*X))
 ENDIF

 END FUNCTION CTNH


