#pragma once
#include <memory>
#include <stdexcept>
#include <string>

#include "formatting_fwd.h"

namespace math {

class Addition;
class Constant;
class Division;
class Multiplication;
class Number;
class Power;
class Subtraction;
class Negate;
class NaturalLog;
class Variable;

// Thrown for un-implemented formatters.
class FormatterNotImplemented : public std::runtime_error {
 public:
  using std::runtime_error::runtime_error;
};

// Define a body for the function that throws. We do this so the base class can be implemented
// piece-meal, rather than being abstract.
#define DEFAULT_FORMAT_IMPL(x) \
  { throw FormatterNotImplemented("Formatter for #x not implemented"); }

#define DECLARE_VIRTUAL_FORMAT_METHOD(Type) \
  virtual void Format(const Type&, StringType&) const DEFAULT_FORMAT_IMPL(Type)

/*
 * Base class for all formatters.
 */
class Formatter {
 public:
  DECLARE_VIRTUAL_FORMAT_METHOD(Addition);
  DECLARE_VIRTUAL_FORMAT_METHOD(Division);
  DECLARE_VIRTUAL_FORMAT_METHOD(Multiplication);
  DECLARE_VIRTUAL_FORMAT_METHOD(Power);
  DECLARE_VIRTUAL_FORMAT_METHOD(Subtraction);

  DECLARE_VIRTUAL_FORMAT_METHOD(Constant);
  DECLARE_VIRTUAL_FORMAT_METHOD(Number);
  DECLARE_VIRTUAL_FORMAT_METHOD(Variable);

  DECLARE_VIRTUAL_FORMAT_METHOD(NaturalLog);
  DECLARE_VIRTUAL_FORMAT_METHOD(Negate);
};

// Simple plain-text formatter.
class PlainFormatter : public Formatter {
 public:
  void Format(const Addition& expr, StringType& output) const override;
  void Format(const Division& expr, StringType& output) const override;
  void Format(const Multiplication& expr, StringType& output) const override;
  void Format(const Power& expr, StringType& output) const override;
  void Format(const Subtraction& expr, StringType& output) const override;

  void Format(const Constant& expr, StringType& output) const override;
  void Format(const Number& expr, StringType& output) const override;
  void Format(const Variable& expr, StringType& output) const override;

  void Format(const NaturalLog& expr, StringType& output) const override;
  void Format(const Negate& expr, StringType& output) const override;
};

}  // namespace math
