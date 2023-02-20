# CMake file fo the python wrapper library.
set(WRAPPER_NAME mc)
pybind11_add_module(${WRAPPER_NAME} wrapper.cc)

# Depend on the static library
add_dependencies(${WRAPPER_NAME} ${PROJECT_NAME})
target_link_libraries(${WRAPPER_NAME} PRIVATE ${PROJECT_NAME})

# Add python tests:
add_subdirectory(python_test)
