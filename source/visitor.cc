// Copyright 2022 Gareth Cross
#include "visitor.h"

#include <fmt/format.h>

#include <string>

// TODO: This won't work on windows, need to disable it on MSVC.
#include <cxxabi.h>

namespace math {

// Demangle type name if possible.
static std::string DemangleTypeName(const char* name) {
  // Demangle the type name.
  int status = 1;
  std::unique_ptr<char, void (*)(void*)> str{abi::__cxa_demangle(name, NULL, NULL, &status),
                                             std::free};
  return std::string{(status == 0) ? str.get() : name};
}

VisitorNotImplemented::VisitorNotImplemented(const char* VisitorName, const char* ArgumentName)
    : what_{fmt::format("Visitor `{}` is missing an implementation for value `{}`.",
                        DemangleTypeName(VisitorName), DemangleTypeName(ArgumentName))} {}

const char* VisitorNotImplemented::what() const noexcept { return what_.c_str(); }

}  // namespace math
