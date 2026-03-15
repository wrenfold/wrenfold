add_py_code_generator(
  bundle_adjustment_factor_generate bundle_adjustment_factor.py
  OUTPUT_FILE_NAME bundle_adjustment_factor.h)

copy_generated_file(GENERATOR_TARGET bundle_adjustment_factor_generate
                    DESTINATION_DIR generated TARGET_GROUP wf_cpp_generation)

# TODO: Add a target that builds gtsam_bal.cc Right now there are too many
# issues with FindGTSAM and FindBoost.
