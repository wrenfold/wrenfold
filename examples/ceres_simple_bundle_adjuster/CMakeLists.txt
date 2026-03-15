add_py_code_generator(
  snavely_reprojection_error_generate snavely_reprojection_error.py
  OUTPUT_FILE_NAME snavely_reprojection_error.generated.h)

copy_generated_file(GENERATOR_TARGET snavely_reprojection_error_generate
                    DESTINATION_DIR generated TARGET_GROUP wf_cpp_generation)

find_package(Ceres QUIET)

if(NOT Ceres_FOUND)
  message(WARNING "Ceres not located, skipping example.")
else()
  add_executable(
    simple_bundle_adjuster simple_bundle_adjuster.cc
                           generated/snavely_reprojection_error.generated.h)
  target_compile_options(simple_bundle_adjuster
                         PRIVATE ${WF_SHARED_WARNING_FLAGS})
  target_compile_definitions(simple_bundle_adjuster
                             PRIVATE -DWF_SPAN_EIGEN_SUPPORT)
  target_link_libraries(simple_bundle_adjuster PRIVATE Ceres::ceres wf_runtime)
  if(NOT MSVC)
    target_compile_options(simple_bundle_adjuster PRIVATE -Wno-unused-parameter)
  endif()
endif()
