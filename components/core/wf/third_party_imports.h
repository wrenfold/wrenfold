// Copyright 2024 Gareth Cross
#pragma once

// Some macros to wrap noisy external imports.
#ifdef _MSC_VER
#define WF_BEGIN_THIRD_PARTY_INCLUDES                                           \
  __pragma(warning(push))               /* push */                              \
      __pragma(warning(disable : 4061)) /* unhandled enum case */               \
      __pragma(warning(disable : 4100)) /* unreferenced parameter */            \
      __pragma(warning(disable : 4127)) /* constant if-statement */             \
      __pragma(warning(disable : 4324)) /* padded for alignment */              \
      __pragma(warning(disable : 4582)) /* constructor not implicitly called */ \
      __pragma(warning(disable : 4583)) /* destructor not implicitly called */
#define WF_END_THIRD_PARTY_INCLUDES __pragma(warning(pop))
#else  // _MSC_VER
#define WF_BEGIN_THIRD_PARTY_INCLUDES
#define WF_END_THIRD_PARTY_INCLUDES
#endif