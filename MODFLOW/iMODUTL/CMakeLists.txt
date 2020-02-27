set(SOURCE
  imod_idf_lib.f90
  imod_idf_mod.f90
  imod_idf_par.f90
  imod_pest_main.f90
  imod_pest_mod.f90
  imod_tseries.f90
  imod_utl.f90
  pks_imod_utl.f90
  pks7mpi.f
  pks7mpimod.f
  pks7mpimodms.f
  pks7mpims.f
  pks7mpiutl.f
  pks7mpiwrp.f
  rf2mf_mod.f90
  rf2mf_mod_conv.f90
  rf2mf_prg.f90
  rf2mf_read.f90
  rf2mf_utl.f90
  rf2ms.f90
  timing.f90
)
if(${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
  set_source_files_properties(
    imod_idf_lib.f90
    imod_utl.f90
    pks7mpi.f
    pks7mpiwrp.f
    PROPERTIES COMPILE_FLAGS -cpp
  )
endif()

add_library(iMODUTL ${SOURCE})

set_property(TARGET iMODUTL
  PROPERTY Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

target_compile_options(iMODUTL
  PRIVATE ${Fortran_WARN_FLAGS}
  PRIVATE ${Fortran_BUILD_FLAGS}
)
