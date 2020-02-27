set(SOURCE
  driver.F90
  driver_bmi.F90
  driver_bmi_module.f90
  driver_module.f90
)

add_library(driver_bmi ${SOURCE})

target_link_libraries(driver_bmi
  PRIVATE
    iMODUTL
	utl
)

target_include_directories(driver_bmi
  PRIVATE
    $<TARGET_PROPERTY:iMODUTL,Fortran_MODULE_DIRECTORY>
    $<TARGET_PROPERTY:utl,Fortran_MODULE_DIRECTORY>
)

target_compile_options(driver_bmi
  PRIVATE ${Fortran_WARN_FLAGS}
  PRIVATE ${Fortran_BUILD_FLAGS}
)
