add_py_code_generator(py_motion_planning problem.py OUTPUT_FILE_NAME
                      generated.rs)

set(BUILD_MOTION_PLANNING ON)
set(MOTION_PLANNING_ENV_VARIABLES "")

if(WIN32)
  # TODO(gareth): Enable this to build on Windows, ideally without vcpkg.
  # https://github.com/gareth-cross/wrenfold/pull/77#issuecomment-1879299785
  set(BUILD_MOTION_PLANNING OFF)
endif()

# A gross hack for linux. openblas-build has a couple of issues: - It pulls in
# TLS/SSL (even when not required). - It fails to set the rpath correctly (so
# does the openssl-src crate). We find both of these with pkg-config, then set
# the `RUSTFLAGS` env variable appropriately. I would rather just fork and fix
# openblas-src.
if(UNIX AND NOT APPLE)
  set(BUILD_MOTION_PLANNING OFF)
  # Check if we have pkg-config:
  if(${PKG_CONFIG_FOUND})
    pkg_check_modules(BLAS openblas)
    pkg_check_modules(SSL openssl)
    if(${BLAS_FOUND} AND ${SSL_FOUND})
      message(STATUS "Found OpenBLAS: ${BLAS_LIBRARY_DIRS}")
      message(STATUS "Found OpenSSL: ${SSL_LIBRARY_DIRS}")
      set(BUILD_MOTION_PLANNING ON)
      set(MOTION_PLANNING_ENV_VARIABLES
          "RUSTFLAGS=-C link-arg=-L${BLAS_LIBRARY_DIRS} \
           -C link-arg=-Wl,-rpath,${BLAS_LIBRARY_DIRS} \
           -C link-arg=-L${SSL_LIBRARY_DIRS} \
           -C link-arg=-Wl,-rpath,${SSL_LIBRARY_DIRS}")
    endif()
  endif()
endif()

if(${BUILD_MOTION_PLANNING})
  add_rust_test(
    motion_planning_test
    CRATE_NAME
    motion_planning_test
    GENERATOR_TARGET
    py_motion_planning
    CRATE_SOURCES
    motion_planning_test/src/lib.rs
    motion_planning_test/src/gen.rs
    motion_planning_test/src/problem.rs
    motion_planning_test/build.rs
    ENV_VARIABLES
    ${MOTION_PLANNING_ENV_VARIABLES})
endif()
