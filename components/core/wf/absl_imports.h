#pragma once

// Shared boilerplate for including absl span + inlined vector types.
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4127)  // conditional expression is constant
#pragma warning(disable : 4100)  // unreferenced formal parameter
#pragma warning(disable : 4324)  // padded for alignment
#endif                           // _MSC_VER
#include <absl/container/inlined_vector.h>
#include <absl/types/span.h>
#ifdef _MSC_VER
#pragma warning(pop)
#endif  //  _MSC_VER
