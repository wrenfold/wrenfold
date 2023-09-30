// Copyright 2023 Gareth Cross
#pragma once

#ifdef _MSC_VER
// Silence some warnings that libfmt can trigger w/ Microsoft compiler.
#pragma warning(push)
#pragma warning(disable : 4583)
#pragma warning(disable : 4582)
#pragma warning(disable : 4061)  // enum/switch
#endif                           // _MSC_VER
#include <fmt/core.h>
#ifdef _MSC_VER
#pragma warning(pop)
#endif  // _MSC_VER
