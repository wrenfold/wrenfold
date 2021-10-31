#pragma once
#include <string>

// Fwd declarations relevant to formatting.
namespace math {

// Forward declare.
class ExpressionBase;
using ExpressionBaseConstPtr = std::shared_ptr<const ExpressionBase>;

#define USE_WIDE_STR  //  Use wide-strings
#ifndef USE_WIDE_STR
using StringType = std::string;
using CharType = char;
#define TEXT(x) (x)
#else
using StringType = std::wstring;
using CharType = wchar_t;
#define TEXT(x) L##x
#endif

// Use PlainFormatter to format an expression.
StringType ToPlainString(const ExpressionBaseConstPtr& expr);

// Convert to plain string from wide string.
std::string NarrowFromWide(const std::wstring& wide_str);

// Convert to wide string from plain string.
std::wstring WideFromNarrow(const std::string& str);

// Performs ToPlainString and then NarrowFromWide.
std::string ToPlainNarrowString(const ExpressionBaseConstPtr& expr);

}  // namespace math
