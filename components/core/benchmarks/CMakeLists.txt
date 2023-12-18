# Function that adds an executable and links it to the `benchmark` framework.
function(add_benchmark SOURCE_FILE)
  # get just the filename w/o extension
  get_filename_component(BENCH_NAME ${SOURCE_FILE} NAME_WE)

  # add test executable for every file
  message(STATUS "Adding benchmark: ${BENCH_NAME}")
  add_executable(${BENCH_NAME} ${SOURCE_FILE})
  target_link_libraries(${BENCH_NAME} wf-core benchmark::benchmark)
  target_compile_options(${BENCH_NAME} PRIVATE ${SHARED_WARNING_FLAGS})
endfunction()

# Add all of our benchmarks:
add_benchmark(bench_add_mul.cc)
add_benchmark(bench_jacobian.cc)
add_benchmark(bench_code_generation.cc)
