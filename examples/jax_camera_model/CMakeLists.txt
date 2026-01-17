if(JAX_FOUND)
  add_python_test(jax_camera_model.py SOURCE_FILES
                  ${WF_EXAMPLES_DIR}/shared_expressions.py)
endif()
