// Copyright 2024 Gareth Cross
#include <gtest/gtest.h>

#include "wf/checked_pointers.h"

namespace wf {

// For testing constructors, we make a fake "smart ptr" type that counts copies and moves.
struct counter_ptr {
  struct counters {
    // Number of times copy constructor and assignment are called.
    std::size_t num_copies{0};
    // Number of times move constructor and move assignment are called.
    std::size_t num_moves{0};
  };

  explicit constexpr counter_ptr(std::nullptr_t) noexcept {}
  constexpr counter_ptr(counters* counters) noexcept : c_(counters) {}

  // Must be convertible to bool for maybe_null ptr to work.
  constexpr operator bool() const noexcept { return static_cast<bool>(c_); }

  counter_ptr(const counter_ptr& other) noexcept : c_(other.c_) {
    if (c_) {
      ++c_->num_copies;
    }
  }

  counter_ptr(counter_ptr&& other) noexcept {
    std::swap(c_, other.c_);
    if (c_) {
      ++c_->num_moves;
    }
  }

  auto& operator=(const counter_ptr& other) noexcept {
    if (this == &other) {
      return *this;
    }
    c_ = other.c_;
    if (c_) {
      ++c_->num_copies;
    }
    return *this;
  }

  auto& operator=(counter_ptr&& other) noexcept {
    if (this == &other) {
      return *this;
    }
    c_ = other.c_;
    other.c_ = nullptr;
    if (c_) {
      ++c_->num_moves;
    }
    return *this;
  }

  constexpr decltype(auto) operator*() const noexcept { return *c_; }

  // Define this for `non_null`:
  constexpr bool operator!=(std::nullptr_t) const noexcept { return c_ != nullptr; }

 private:
  counters* c_{nullptr};
};

static_assert(!std::is_trivially_move_assignable_v<counter_ptr> &&
              !std::is_trivially_move_constructible_v<counter_ptr> &&
              !std::is_trivially_copyable_v<counter_ptr> &&
              std::is_nothrow_move_constructible_v<counter_ptr> &&
              std::is_nothrow_copy_constructible_v<counter_ptr>);

struct foo {};

TEST(NonNullTest, TestNullConstruction) {
  foo* bad_ptr = nullptr;
  const auto create_null = [&] { non_null<foo*> thing(bad_ptr); };
  const auto create_const_null = [&] { non_null<const foo*> thing(bad_ptr); };
  ASSERT_THROW(create_null(), wf::assertion_error);
  ASSERT_THROW(create_const_null(), wf::assertion_error);
  static_assert(!noexcept(non_null<foo*>(bad_ptr)));
  static_assert(!noexcept(non_null<const foo*>(bad_ptr)));

  static_assert(std::is_same_v<foo*, decltype(std::declval<non_null<foo*>>().get())>);
  static_assert(std::is_same_v<const foo*, decltype(std::declval<non_null<const foo*>>().get())>);

  static_assert(!std::is_default_constructible_v<non_null<foo>>);
  static_assert(!std::is_default_constructible_v<non_null<const foo>>);
}

// Check that constructors copy + move appropriately.
TEST(NonNullTest, TestConstructors) {
  counter_ptr::counters count{};

  // Construct from underlying ptr:
  non_null<counter_ptr> n{&count};
  ASSERT_TRUE(n);
  ASSERT_NE(n, nullptr);
  ASSERT_NE(nullptr, n);
  ASSERT_EQ(n, n);
  ASSERT_EQ(&count, std::addressof(*n));
  ASSERT_EQ(0, count.num_copies);
  ASSERT_EQ(0, count.num_moves);

  // Return by reference for non-trivially copyable type:
  static_assert(std::is_same_v<const counter_ptr&, decltype(n.get())>);
  static_assert(std::is_same_v<const counter_ptr&, decltype(n.operator->())>);
  static_assert(std::is_same_v<counter_ptr::counters&, decltype(n.operator*())>);

  static_assert(std::is_nothrow_copy_constructible_v<decltype(n)>);
  static_assert(std::is_nothrow_copy_assignable_v<decltype(n)>);
  static_assert(std::is_nothrow_move_constructible_v<decltype(n)>);
  static_assert(std::is_nothrow_move_assignable_v<decltype(n)>);

  static_assert(!std::is_default_constructible_v<non_null<counter_ptr>>);
  static_assert(!std::is_default_constructible_v<non_null<const counter_ptr>>);

  // Copy:
  non_null<counter_ptr> n2{n};
  ASSERT_TRUE(n2);
  ASSERT_EQ(&count, std::addressof(*n2));
  ASSERT_EQ(1, count.num_copies);
  ASSERT_EQ(0, count.num_moves);

  // Move:
  non_null<counter_ptr> n3{std::move(n2)};
  ASSERT_FALSE(n2);  //  n2 is empty
  ASSERT_TRUE(n3);
  ASSERT_NE(n2, n3);
  ASSERT_EQ(&count, std::addressof(*n3));
  ASSERT_EQ(n3, n);
  ASSERT_EQ(1, count.num_copies);
  ASSERT_EQ(1, count.num_moves);

  // Copy-into-const
  non_null<const counter_ptr> n4{n3};
  ASSERT_TRUE(n4);
  ASSERT_EQ(n3, n4);
  ASSERT_EQ(&count, std::addressof(*n4));
  ASSERT_EQ(2, count.num_copies);
  ASSERT_EQ(2, count.num_moves);  //  Moved by copy-constructor.

  // Move-into const
  non_null<const counter_ptr> n5{std::move(n3)};
  ASSERT_FALSE(n3);  //  n3 is empty.
  ASSERT_TRUE(n5);
  ASSERT_EQ(n, n5);
  ASSERT_EQ(2, count.num_copies);
  ASSERT_EQ(4, count.num_moves);  //  Moved twice by converting constructor.
}

TEST(NonNullTest, TestHash) {
  int x = 0;
  non_null<int*> ptr{&x};
  ASSERT_EQ(std::hash<int*>{}(&x), std::hash<non_null<int*>>{}(ptr));
}

TEST(MaybeNullTest, TestNullConstruction) {
  maybe_null<foo*> empty{nullptr};
  ASSERT_FALSE(empty);
  ASSERT_FALSE(empty.has_value());
  ASSERT_EQ(nullptr, empty.get_unchecked());

  // Should throw when accessed.
  ASSERT_THROW(empty.get(), wf::assertion_error);
  ASSERT_THROW(*empty, wf::assertion_error);
  ASSERT_THROW(empty.operator->(), wf::assertion_error);

  static_assert(!std::is_default_constructible_v<maybe_null<foo*>>);
  static_assert(std::is_same_v<foo*, decltype(empty.get_unchecked())>);
  static_assert(std::is_same_v<foo*, decltype(empty.get())>);
  static_assert(noexcept(empty.get_unchecked()));
  static_assert(!noexcept(empty.get()));
  static_assert(!noexcept(empty.operator*()));
  static_assert(!noexcept(empty.operator->()));

  // Gheck that we get the right types for const pointers:
  maybe_null<const foo*> empty_const{nullptr};
  ASSERT_FALSE(empty_const);
  ASSERT_FALSE(empty_const.has_value());
  static_assert(std::is_same_v<const foo*, decltype(empty_const.get_unchecked())>);
  static_assert(std::is_same_v<const foo*, decltype(empty_const.get())>);

  // Const and non-const should be comparable:
  ASSERT_EQ(empty, empty_const);
}

TEST(MaybeNullTest, TestConstructors) {
  counter_ptr::counters count{};

  // Conversion construction:
  maybe_null<counter_ptr> m{&count};
  ASSERT_TRUE(m);
  ASSERT_EQ(&count, &m.operator*());
  ASSERT_EQ(0, count.num_copies);
  ASSERT_EQ(0, count.num_moves);

  // Should return by reference, since counter_ptr is not trivially copyable.
  static_assert(std::is_same_v<const counter_ptr&, decltype(m.get())>);
  static_assert(std::is_same_v<const counter_ptr&, decltype(m.operator->())>);
  static_assert(std::is_same_v<counter_ptr::counters&, decltype(m.operator*())>);
  static_assert(noexcept(m.get_unchecked()));
  static_assert(!noexcept(m.get()));

  // All the copy/move operators on counter_ptr are nothrow.
  static_assert(std::is_nothrow_copy_constructible_v<maybe_null<counter_ptr>>);
  static_assert(std::is_nothrow_copy_assignable_v<maybe_null<counter_ptr>>);
  static_assert(std::is_nothrow_move_constructible_v<maybe_null<counter_ptr>>);
  static_assert(std::is_nothrow_move_assignable_v<maybe_null<counter_ptr>>);

  // Copy construct:
  maybe_null m2{m};
  ASSERT_TRUE(m2);
  ASSERT_TRUE(m2.has_value());
  ASSERT_EQ(&count, &m2.operator*());
  ASSERT_EQ(1, count.num_copies);
  ASSERT_EQ(0, count.num_moves);

  // Copy assign:
  maybe_null<counter_ptr> m3{nullptr};
  m3 = m;
  ASSERT_TRUE(m3);
  ASSERT_EQ(&count, &m3.operator*());
  ASSERT_EQ(2, count.num_copies);
  ASSERT_EQ(0, count.num_moves);

  // Move construct:
  maybe_null<counter_ptr> m4{std::move(m)};
  ASSERT_TRUE(m4);
  ASSERT_FALSE(m);  //  `m` is now empty
  ASSERT_EQ(&count, &m4.operator*());
  ASSERT_EQ(2, count.num_copies);
  ASSERT_EQ(1, count.num_moves);

  // Move assign:
  maybe_null<counter_ptr> m5{nullptr};
  m5 = std::move(m2);
  ASSERT_TRUE(m5);
  ASSERT_FALSE(m2);  //  `m2` is now empty
  ASSERT_EQ(&count, &m5.operator*());
  ASSERT_EQ(2, count.num_copies);
  ASSERT_EQ(2, count.num_moves);
  ASSERT_NE(m5, m2);

  // Move construct const:
  maybe_null<const counter_ptr> m6{std::move(m4)};
  ASSERT_TRUE(m6);
  ASSERT_FALSE(m4);  //  `m4` is now empty
  ASSERT_EQ(&count, &m6.operator*());
  ASSERT_EQ(2, count.num_copies);
  ASSERT_EQ(4, count.num_moves);  // Moved twice: once from m4, then inside m6 constructor.
  ASSERT_EQ(m6, m5);
  ASSERT_NE(m6, m4);  // Not equal because m4 is empty.

  // Copy construct const:
  maybe_null<const counter_ptr> m7{m5};
  ASSERT_TRUE(m7);
  ASSERT_TRUE(m5);  //  m5 still exists
  ASSERT_EQ(&count, &m7.operator*());
  ASSERT_EQ(3, count.num_copies);
  ASSERT_EQ(5, count.num_moves);  //  Moved inside the constructor of m7.
  ASSERT_EQ(m7, m5);
}

TEST(MaybeNullTest, TestHash) {
  int x = 0;
  maybe_null<int*> ptr{&x};
  ASSERT_EQ(std::hash<int*>{}(&x), std::hash<maybe_null<int*>>{}(ptr));
}

TEST(MaybeNullTest, TestWithSharedPtr) {
  using int_shared_ptr = std::shared_ptr<const int>;
  auto underlying_ptr = std::make_shared<int>(7);

  maybe_null<int_shared_ptr> m{underlying_ptr};
  ASSERT_EQ(2, underlying_ptr.use_count());
  ASSERT_EQ(underlying_ptr.get(), std::addressof(*m));
  ASSERT_EQ(underlying_ptr.get(), m.get().get());
  ASSERT_EQ(7, *m);

  static_assert(std::is_same_v<const int_shared_ptr&, decltype(m.get())>);
  static_assert(std::is_same_v<const int_shared_ptr&, decltype(m.operator->())>);
  static_assert(std::is_same_v<const int&, decltype(m.operator*())>);
  static_assert(!noexcept(m.get()));

  // shared-ptrs can be moved and copied without throwing
  static_assert(std::is_nothrow_copy_constructible_v<decltype(m)>);
  static_assert(std::is_nothrow_copy_assignable_v<decltype(m)>);
  static_assert(std::is_nothrow_move_constructible_v<decltype(m)>);
  static_assert(std::is_nothrow_move_assignable_v<decltype(m)>);

  // Non-const --> const should be nothrow for shared_ptr
  static_assert(std::is_nothrow_constructible_v<decltype(m), maybe_null<std::shared_ptr<int>>>);

  // Copy it:
  maybe_null m2 = m;
  ASSERT_TRUE(m2);
  ASSERT_EQ(3, underlying_ptr.use_count());
  ASSERT_EQ(underlying_ptr.get(), std::addressof(*m2));

  // Move it:
  maybe_null m3{std::move(m2)};
  ASSERT_TRUE(m3);
  ASSERT_FALSE(m2);
  ASSERT_EQ(3, underlying_ptr.use_count());
  ASSERT_EQ(underlying_ptr.get(), std::addressof(*m3));
}

TEST(MaybeNullTest, TestWithUniquePtr) {
  using int_unique_ptr = std::unique_ptr<const int>;

  maybe_null<int_unique_ptr> m{std::make_unique<int>(13)};
  ASSERT_TRUE(m);
  ASSERT_EQ(13, *m);

  static_assert(std::is_same_v<const int_unique_ptr&, decltype(m.get())>);
  static_assert(std::is_same_v<const int_unique_ptr&, decltype(m.operator->())>);
  static_assert(std::is_same_v<const int&, decltype(m.operator*())>);
  static_assert(noexcept(m.get_unchecked()));
  static_assert(!noexcept(m.get()));

  // unique-ptrs can be moved but not copied
  static_assert(!std::is_nothrow_copy_constructible_v<decltype(m)>);
  static_assert(!std::is_nothrow_copy_assignable_v<decltype(m)>);
  static_assert(std::is_nothrow_move_constructible_v<decltype(m)>);
  static_assert(std::is_nothrow_move_assignable_v<decltype(m)>);

  // move construct it:
  maybe_null m2{std::move(m)};
  ASSERT_TRUE(m2);
  ASSERT_FALSE(m);
  ASSERT_EQ(13, *m2);

  // move assign it:
  maybe_null<int_unique_ptr> m3{nullptr};
  ASSERT_FALSE(m3);
  m3 = std::move(m2);
  ASSERT_TRUE(m3);
  ASSERT_FALSE(m2);
  ASSERT_EQ(13, *m3);
}

TEST(MaybeNullTest, TestWithThrowingPtr) {
  // catapult_ptr likes to throw, sometimes.
  struct catapult_ptr {
    catapult_ptr(std::nullptr_t) noexcept(false) { throw std::runtime_error("null construct"); }
    catapult_ptr(int*) noexcept(false) { throw std::runtime_error("int construct"); }
    catapult_ptr(const catapult_ptr&) noexcept(false) {
      throw std::runtime_error("copy construct");
    }
    catapult_ptr(catapult_ptr&&) noexcept = default;

    // move and copy assign are no-throw
    catapult_ptr& operator=(const catapult_ptr&) noexcept = default;
    catapult_ptr& operator=(catapult_ptr&&) noexcept = default;

    operator bool() const noexcept(false) { throw std::runtime_error("bool convert"); }
  };

  // All these constructors throw:
  using maybe_null_catapult = maybe_null<catapult_ptr>;
  static_assert(!noexcept(maybe_null_catapult(nullptr)));
  static_assert(!noexcept(maybe_null_catapult(std::declval<int*>())));
  static_assert(!noexcept(maybe_null_catapult(std::declval<catapult_ptr&>())));

  // Move constructor does not throw:
  static_assert(noexcept(maybe_null_catapult(std::declval<catapult_ptr&&>())));

  // Copy and move assignment do not throw:
  static_assert(
      noexcept(std::declval<maybe_null_catapult>() = std::declval<const maybe_null_catapult&>()));
  static_assert(
      noexcept(std::declval<maybe_null_catapult>() = std::declval<maybe_null_catapult>()));

  // Checking for value throws:
  static_assert(!noexcept(std::declval<maybe_null_catapult>().has_value()));

  // Non-trivially copyable, so this converts to ref:
  static_assert(noexcept(std::declval<maybe_null_catapult>().get_unchecked()));
}

}  // namespace wf
