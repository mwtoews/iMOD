MODULE MOD_ASC2IDF_UTL

USE MOD_ASC2IDF_PAR

CONTAINS

 !###======================================================================
 SUBROUTINE ASC2IDF_INT_NULLIFY()
 !###======================================================================
 IMPLICIT NONE
 
 NULLIFY(XP,YP,ZP,WP,FP)
  
 END SUBROUTINE ASC2IDF_INT_NULLIFY

 !###======================================================================
 SUBROUTINE ASC2IDF_INT_DEALLOCATE()
 !###======================================================================
 IMPLICIT NONE
 
 IF(ASSOCIATED(XP))DEALLOCATE(XP)
 IF(ASSOCIATED(YP))DEALLOCATE(YP) 
 IF(ASSOCIATED(ZP))DEALLOCATE(ZP) 
 IF(ASSOCIATED(WP))DEALLOCATE(WP) 
 IF(ASSOCIATED(FP))DEALLOCATE(FP) 
  
 END SUBROUTINE ASC2IDF_INT_DEALLOCATE
 
 !###======================================================================
 SUBROUTINE ASC2IDF_INT_RESIZEVECTORS(N,DN)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N,DN
 INTEGER :: MM,NN,I
 
 !## make sure vector is equal to entered abs(n)
 IF(DN.EQ.0)THEN
  MM=N; NN=N; IF(NN.EQ.SIZE(XP))RETURN; NN=SIZE(XP)
 ELSE 
  NN=SIZE(XP); IF(N.LE.NN)RETURN; MM=NN+DN
 ENDIF
 
 ALLOCATE(XP_DUMMY(MM),YP_DUMMY(MM),ZP_DUMMY(MM),WP_DUMMY(MM),FP_DUMMY(MM))
 
 XP_DUMMY=0.0D0
 YP_DUMMY=0.0D0
 ZP_DUMMY=0.0D0
 WP_DUMMY=0.0D0
 FP_DUMMY=0.0D0

 DO I=1,MIN(MM,NN)
  XP_DUMMY(I)=XP(I)
  YP_DUMMY(I)=YP(I)
  ZP_DUMMY(I)=ZP(I)
  WP_DUMMY(I)=WP(I)
  FP_DUMMY(I)=FP(I)
 ENDDO
 
 DEALLOCATE(XP,YP,ZP,WP,FP)
 
 XP=>XP_DUMMY
 YP=>YP_DUMMY
 ZP=>ZP_DUMMY
 WP=>WP_DUMMY
 FP=>FP_DUMMY
 
 END SUBROUTINE ASC2IDF_INT_RESIZEVECTORS

END MODULE MOD_ASC2IDF_UTL