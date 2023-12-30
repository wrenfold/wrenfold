// Copyright 2023 Gareth Cross
#define PYBIND11_DETAILED_ERROR_MESSAGES
#include <pybind11/functional.h>
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include "wf/code_generation/ast.h"
#include "wf/code_generation/ast_formatters.h"
#include "wf/code_generation/cpp_code_generator.h"
#include "wf/code_generation/rust_code_generator.h"

namespace py = pybind11;
using namespace py::literals;

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4250)  //  inheritance via dominance
#endif

namespace wf {

// define_override implements an override to the `std::string operator(const T&)` method.
// We don't need a constructor here because we inherit virtually from Base. We use virtual
// inheritance because define_override is inherited once for every type we can format.
template <typename Derived, typename Base, typename T>
class define_override : public virtual Base {
 public:
  std::string operator()(const T& element) const override final {
    // Downcast to our derived type so that we can check if this override exists.
    return static_cast<const Derived&>(*this).maybe_override(element);
  }
};

// define_all_overrides is specialized on a type_list, and will inherit from `define_override`
// once for every type in the type list. This allows us to automatically implement an override for
// operator(), without having to use a macro or manually define all the overrides.
template <typename Derived, typename Base, typename T>
class define_all_overrides;
template <typename Derived, typename Base, typename... Ts>
class define_all_overrides<Derived, Base, type_list<Ts...>>
    : public define_override<Derived, Base, Ts>... {
 public:
  // We do need a constructor here because we inherit non-virtually from
  // define_override. This needs to be a non virtual inheritance so that
  // define_override can downcast to the derived type. static_cast<> needs to
  // know the memory layout of the class in order to return a valid pointer.
  // If we used virtual inheritance with define_override, that would not be
  // possible.
  //
  // https://stackoverflow.com/questions/7484913/
  template <typename... CtorArgs>
  explicit define_all_overrides(CtorArgs&&... args) : Base(std::forward<CtorArgs>(args)...) {}
};

// pybind11 has issues with combining recursion and inheritance.
// For example: https://github.com/pybind/pybind11/issues/1552
//
// The root of the issue AFAIK is that pybind detetermines whether to call `self.method` or
// `super().method` by inspecting the python stack in order to check if the overriden method is
// calling itself. This makes formatting a recursive structure somewhat tricky. For instance,
// consider the expression `cos(sqrt(x))`. This entails the `format_call` function calling itself in
// order to obtain the formatted argument to `cos`. pybind11 incorrectly detects this as attempt to
// call the super method in C++, rather than respecting the user override. This behavior appears to
// differ in `ipdb` vs regular python (maybe since the call stack is altered by the presence of the
// debugger).
//
// To work around this, we manually check for overrides (in a similar manner to pybind11, by using
// getattr). Rather than trying to automatically select `super` or `self`, we expose both methods
// under different names.
template <typename Base>
class wrapped_generator
    : public define_all_overrides<wrapped_generator<Base>, Base, ast::all_ast_types> {
 public:
  using generator_base = Base;
  using override_base = define_all_overrides<wrapped_generator, Base, ast::all_ast_types>;

  // This scenario is a bit confusing, but basically what will happen here is:
  // - The constructor for Base is called (once).
  // - The constructor for `override_base` is called.
  //
  // https://isocpp.org/wiki/faq/multiple-inheritance#virtual-inheritance-ctors
  //
  // Quote: Because a virtual base class subobject occurs only once in an
  // instance, there are special rules to make sure the virtual base class’s
  // constructor and destructor get called exactly once per instance. The C++
  // rules say that virtual base classes are constructed before all
  // non-virtual base classes. The thing you as a programmer need to know is
  // this: constructors for virtual base classes anywhere in your class’s
  // inheritance hierarchy are called by the “most derived” class’s
  // constructor.
  //
  // The args to `override_base` do not matter, because we already invoked
  // Base(...). The types do need to match in order to compile, but Base(...)
  // is invoked only once.
  template <typename... CtorArgs>
  explicit wrapped_generator(CtorArgs&&... args)
      : Base(std::forward<CtorArgs>(args)...),
        // These value of these args don't matter, because Base(...) is invoked only once.
        // The types do need to match though in order to compile, since override_base
        // must appear to invoke the Base constructor correctly, and Base is not
        // default constructible.
        override_base(std::forward<CtorArgs>(args)...) {
    recursions_.fill(0);
  }

  // Check if a python class derived from this one implements a formatting operator for type `T`.
  // If it does, we call the derived class implementation. Otherwise, call Base::operator().
  //
  // This method is loosely inspired by pybind11 get_override.
  template <typename T>
  std::string maybe_override(const T& element) const {
    /* scope for acquiring the GIL */ {
      py::gil_scoped_acquire gil;
      if (const py::function func = get_override<T>(); static_cast<bool>(func)) {
        try {
          using std_function = std::function<std::string(const T&)>;
          const std_function typed_func = py::cast<std_function>(func);
          return typed_func(element);
        } catch (const py::type_error& err) {
          throw type_error(
              "Failed while casting formatter `format_{snek}` to std::function<std::string(const "
              "{snek}&)>. The python method should have the signature: format_{snek}(element: "
              "{camel}) -> str\n"
              "pybind11 error: {err}",
              fmt::arg("snek", T::snake_case_name_str),
              fmt::arg("camel", ast::camel_case_name<T>()), fmt::arg("err", err.what()));
        }
      }
    }
    // Fall back to base class implementation.
    return static_cast<const Base&>(*this).Base::operator()(element);
  }

  // Invoke the base class operator() and check for infinite recursions.
  template <typename T>
  std::string invoke_with_guard(const T& element) const {
    recursion_guard guard{recursions_, element};
    return std::invoke(*this, element);
  }

 private:
  // Search the derived class for a formatting method that accepts type `T`.
  // Return it as a py::function (which will be empty if we cannot find an appropriate method).
  // First check the cache, but if the cache is empty we look for the method using getattr().
  template <typename T>
  py::function get_override() const {
    if (const auto it = has_override_.find(typeid(T)); it != has_override_.end()) {
      if (it->second) {
        return getattr_override<T>();
      } else {
        return py::function();
      }
    }
    py::function override = getattr_override<T>();
    has_override_.emplace(typeid(T), static_cast<bool>(override));
    return override;
  }

  // Check if our derived type has an appropriate override for type `T`.
  // If it does, stash it in our cache. If not, place an empty entry into the cache so that we do
  // not repeat this work.
  template <typename T>
  py::function getattr_override() const {
    static const std::string method_name = fmt::format("format_{}", T::snake_case_name_str);
    const py::object maybe_method = py::getattr(
        py::cast(*this, pybind11::return_value_policy::reference), method_name.c_str(), py::none());
    if (maybe_method.is_none()) {
      return py::function();
    }
    // Object is not none, so it must be a callable.
    if (!py::isinstance<py::function>(maybe_method)) {
      const py::str type_repr = py::repr(py::type::of(maybe_method));
      throw type_error(
          "The formatter attribute `{}` should be a callable object. Instead we found: `{}`",
          method_name, static_cast<std::string>(type_repr));
    }
    return py::cast<py::function>(maybe_method);
  }

  // We need mutable here because operator() is const in the C++ classes.
  // This should be thread safe since the GIL prevents access from multiple threads.
  // I'd prefer to save weak handles to the underlying functions, but I can't quite figure out how
  // to do that in pybind11 without hitting invalid access. For now just cache whether the override
  // exists.
  mutable std::unordered_map<std::type_index, bool> has_override_;

  // Store the recursion count for each type.
  using recursion_counters = std::array<std::int32_t, type_list_size_v<ast::all_ast_types>>;
  mutable recursion_counters recursions_;

  // Because we know the ast tree is not very deep, we can catch infinite recursions by counting
  // recursions and throwing when we pass a relatively low threshold. This type increases the count
  // on construction, and descreases it on destruction.
  class recursion_guard {
   public:
    template <typename T>
    explicit recursion_guard(recursion_counters& counters, const T&)
        : counter_(counters[index_of_type_v<T, ast::all_ast_types>]) {
      if (constexpr std::int32_t max_recursions = 32; counter_ + 1 == max_recursions) {
        throw std::runtime_error(
            fmt::format("Recursed {} times while formatting type `{}`. It is possible that a "
                        "formatting override specified in python is calling itself indirectly. "
                        "Instead of calling format(), you likely intended to call super_format().",
                        max_recursions, ast::camel_case_name<T>()));
      }
      ++counter_;
    }

    ~recursion_guard() { counter_ = std::max(counter_ - 1, 0); }

   private:
    std::int32_t& counter_;
  };
};

// This struct expands over all the types in `ast::variant` and exposes operator() for
// all of them via pybind11.
template <typename T = ast::all_ast_types>
struct register_operators_struct;
template <typename... Ts>
struct register_operators_struct<type_list<Ts...>> {
  template <typename T, typename PyClass>
  static void register_operator(PyClass& klass) {
    // Static const so that we are certain pybind11 isn't taking an invalid weak reference here.
    static const std::string doc = fmt::format("Format ast type `{}`.", ast::camel_case_name<T>());

    // Expose operator() on the generator.
    // underlying_base_type will be cpp_code_generator, etc...
    using wrapped_type = typename PyClass::type;
    using underlying_base_type = typename wrapped_type::generator_base;

    // Register under an overloaded name.
    // This is so we can just call format(...) in python, and have pybind11 automatically
    // do the overload resolution.
    klass.def(
        "format",
        [](const wrapped_type& self, const T& arg) -> std::string {
          return self.invoke_with_guard(arg);
        },
        py::arg("element"), py::doc(doc.c_str()));

    // super_format always calls the base class implementation.
    klass.def(
        "super_format",
        [](const wrapped_type& self, const T& arg) -> std::string {
          return self.underlying_base_type::operator()(arg);
        },
        py::arg("element"), py::doc(doc.c_str()));
  }

  template <typename PyClass>
  void operator()(PyClass& klass) {
    // Expand over all the types `Ts`:
    (register_operator<Ts, PyClass>(klass), ...);
  }
};

// Generate multiple definitions.
template <typename Generator>
std::string generate_multiple(const Generator& generator,
                              const std::vector<ast::function_definition>& definitions) {
  if (definitions.empty()) {
    return "";
  }
  auto it = definitions.begin();
  std::string output = std::invoke(generator, *it);
  for (++it; it != definitions.end(); ++it) {
    output.append("\n\n");
    output += std::invoke(generator, *it);
  }
  return output;
}

// Wrap generator of type `T`.
template <typename T, typename... CtorArgs>
static auto wrap_code_generator(py::module_& m, const std::string_view name) {
  py::class_ klass =
      py::class_<wrapped_generator<T>>(m, name.data())
          .def(py::init<CtorArgs...>())
          .def(
              "generate",
              [](const wrapped_generator<T>& self, const ast::function_definition& definition) {
                return std::invoke(self, definition);
              },
              py::arg("definition"), py::doc("Generate code for the provided definition."))
          .def("generate", &generate_multiple<wrapped_generator<T>>, py::arg("definitions"),
               py::doc("Generate code for multiple definitions."));
  // Wrap all the operator() methods.
  register_operators_struct{}(klass);
  return klass;
}

// Search all the types [T, Ts...] to see if one of them matches `type`.
// If `T` matches `type`, we return its type_index. Otherwise, we continue the search.
// TODO: Cache this result instead of doing linear search.
template <typename T, typename... Ts>
bool is_formattable_type(const py::type& type) {
  if (const py::type candidate = py::type::of<T>(); type.is(candidate)) {
    return true;
  }
  if constexpr (sizeof...(Ts) > 0) {
    return is_formattable_type<Ts...>(type);
  } else {
    return false;
  }
}
// Unpack the variadic std::variant template and invoke `is_formattable_type`.
template <typename T = ast::all_ast_types>
struct is_formattable_type_struct;
template <typename... Ts>
struct is_formattable_type_struct<type_list<Ts...>> {
  auto operator()(const py::type& type) const { return is_formattable_type<Ts...>(type); }
};

void wrap_code_formatting_operations(py::module_& m) {
  wrap_code_generator<cpp_code_generator>(m, "CppGenerator");
  wrap_code_generator<rust_code_generator>(m, "RustGenerator");
  m.def(
      "is_formattable_type",
      [](const py::type& type) { return is_formattable_type_struct<>{}(type); }, py::arg("t"),
      py::doc("Check if the provided type is formattable with a code generator."));
}

}  // namespace wf

#ifdef _MSC_VER
#pragma warning(pop)
#endif
