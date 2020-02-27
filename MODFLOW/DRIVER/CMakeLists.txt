set(SOURCE
  driver.F90
  driver_module.f90
  metaswap_comp.FOR
  mozart_comp.f90
  transol_comp.FOR
)

add_executable(driver ${SOURCE})

target_link_libraries(driver
  PRIVATE
    iMODUTL
	mf2005
	utl
)

target_include_directories(driver
  PRIVATE
    $<TARGET_PROPERTY:iMODUTL,Fortran_MODULE_DIRECTORY>
    $<TARGET_PROPERTY:utl,Fortran_MODULE_DIRECTORY>
)

target_compile_options(driver
  PRIVATE ${Fortran_WARN_FLAGS}
  PRIVATE ${Fortran_BUILD_FLAGS}
)
