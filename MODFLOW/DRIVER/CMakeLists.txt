set(SOURCE
  driver.F90
  driver_module.f90
  metaswap_comp.FOR
  mozart_comp.f90
  transol_comp.FOR
)

add_executable(driver ${SOURCE})

target_link_libraries(driver
  PUBLIC
    iMODUTL
	mf2005
	UTL
)

target_include_directories(driver
  PUBLIC
    $<TARGET_PROPERTY:iMODUTL,Fortran_MODULE_DIRECTORY>
    $<TARGET_PROPERTY:UTL,Fortran_MODULE_DIRECTORY>
)
