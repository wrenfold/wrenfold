# Record the current binary dir for use in downstream files:
set(COMPONENTS_BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR})
set(COMPONENTS_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR})

add_subdirectory(math)
add_subdirectory(sym_wrapper)
add_subdirectory(runtime)
