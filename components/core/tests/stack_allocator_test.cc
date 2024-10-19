// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#include <gtest/gtest.h>

#include <algorithm>  // std::reverse
#include <list>
#include <vector>

#include "wf/utility/stack_allocator.h"

namespace wf {

static_assert(4 == internal::round_to_alignment(4, 1));
static_assert(4 == internal::round_to_alignment(4, 4));
static_assert(8 == internal::round_to_alignment(8, 4));
static_assert(16 == internal::round_to_alignment(16, 1));
static_assert(32 == internal::round_to_alignment(16, 32));
static_assert(32 == internal::round_to_alignment(32, 3));

struct block {
  void* ptr;
  std::size_t n;
};

template <std::size_t Capacity, std::size_t Alignment>
void test_with_alignment() {
  stack_allocator<Capacity, Alignment> s{};
  EXPECT_FALSE(s.owns(nullptr));
  EXPECT_TRUE(s.is_empty());

  static_assert(!std::is_copy_assignable_v<decltype(s)>);
  static_assert(!std::is_move_assignable_v<decltype(s)>);
  static_assert(!std::is_copy_constructible_v<decltype(s)>);
  static_assert(!std::is_move_constructible_v<decltype(s)>);

  constexpr std::size_t max_allocations = Capacity / Alignment;
  static_assert(max_allocations > 1);

  std::vector<block> pointers;
  for (std::size_t i = 0; i < max_allocations; ++i) {
    // Vary the sizes we allocate, but keep them all below `Alignment`.
    const std::size_t n = std::max<std::size_t>(i % Alignment, 1);
    auto ptr = s.allocate(n);
    EXPECT_NE(nullptr, ptr);
    EXPECT_EQ(0, reinterpret_cast<std::ptrdiff_t>(ptr) % Alignment);
    EXPECT_EQ(Alignment * (i + 1), s.used_bytes());
    EXPECT_TRUE(s.owns(ptr) && s.in_live_range(ptr));
    pointers.push_back(block{ptr, n});
  }

  // Should now be full:
  ASSERT_EQ(nullptr, s.allocate(1));

  // Trying to remove one that was not at the back should do nothing:
  for (std::size_t i = 0; i + 1 < pointers.size(); ++i) {
    s.deallocate(pointers[i].ptr, pointers[i].n);
    EXPECT_TRUE(s.in_live_range(pointers[i].ptr));  //  Was not freed.
  }

  // still full:
  ASSERT_EQ(nullptr, s.allocate(1));

  // Now free all the blocks:
  std::reverse(pointers.begin(), pointers.end());
  for (std::size_t i = 0; i < pointers.size(); ++i) {
    s.deallocate(pointers[i].ptr, pointers[i].n);
    // Still owned, but no longer in live range:
    EXPECT_TRUE(s.owns(pointers[i].ptr));
    EXPECT_FALSE(s.in_live_range(pointers[i].ptr));
  }

  ASSERT_TRUE(s.is_empty());

  // Should be empty, so check that we can fill it back up:
  for (std::size_t i = 0; i < max_allocations; ++i) {
    auto ptr = s.allocate(1);
    EXPECT_NE(nullptr, ptr);
    EXPECT_EQ(0, reinterpret_cast<std::ptrdiff_t>(ptr) % Alignment);
    EXPECT_TRUE(s.owns(ptr) && s.in_live_range(ptr));
  }

  // full again:
  EXPECT_EQ(nullptr, s.allocate(1));
}

// Test the stack allocator with different capacities and alignments.
TEST(StackAllocatorTest, TestStackAllocator) {
  test_with_alignment<40, 16>();
  test_with_alignment<64, 4>();
  test_with_alignment<64, 8>();
  test_with_alignment<80, 16>();
  test_with_alignment<128, 16>();
  test_with_alignment<128, 32>();
  test_with_alignment<999, 16>();
  test_with_alignment<1024, 64>();
}

TEST(StackAllocatorTest, TestVector1) {
  using allocator = stl_stack_allocator_with_fallback<int, sizeof(int) * 8>;
  static_assert(alignof(int) == 4);

  allocator::stack_allocator_type s;
  std::vector<int, allocator> v{allocator(s)};

  // Perform an allocation that should fit in the stack.
  v.resize(2);
  ASSERT_TRUE(s.owns(&v[0]));

  // This may fail, but clang/gcc/msvc _should_ all deallocate when size is zero.
  v.clear();
  v.shrink_to_fit();

  ASSERT_FALSE(s.owns(v.data()));
  ASSERT_TRUE(s.is_empty());

  v.resize(4, 1984);
  ASSERT_TRUE(s.owns(&v[0]));

  // exceed the size of the stack allocator
  for (std::size_t i = 0; i < 10; ++i) {
    v.push_back(static_cast<int>(i));
  }
  ASSERT_FALSE(s.owns(v.data()));

  // Clear and push back again
  v.clear();
  v.shrink_to_fit();

  ASSERT_FALSE(s.owns(v.data()));
  ASSERT_TRUE(s.is_empty());

  v.push_back(42);
  ASSERT_TRUE(s.owns(&v[0]));

  // We should be able to move the vector:
  std::vector<int, allocator> v_moved{std::move(v)};
  v.clear();
  ASSERT_TRUE(s.owns(v_moved.data()));
  ASSERT_FALSE(s.owns(v.data()));

  // And copy it using the same allocator.
  // Place this in a scope so it is destroyed upon exit.
  {
    const std::vector<int, allocator> v_copied{v_moved};
    ASSERT_TRUE(s.owns(v_copied.data()));  //  The allocator was used by the copy.
  }
  ASSERT_FALSE(s.is_empty());

  // Now clear the moved vector, which should free up the whole stack.
  v_moved.clear();
  v_moved.shrink_to_fit();
  ASSERT_TRUE(s.is_empty());
}

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4324)  //  padded for alignment.
#endif                           // _MSC_VER

// A test struct that prefers 32-byte alignment.
struct alignas(32) aligned_int {
  int v;
  explicit constexpr aligned_int(int v) noexcept : v(v) {}
};
static_assert(alignof(aligned_int) == 32);

#ifdef _MSC_VER
#pragma warning(pop)
#endif  // _MSC_VER

TEST(StackAllocatorTest, TestVector2) {
  using allocator = stl_stack_allocator_with_fallback<aligned_int, sizeof(aligned_int) * 8>;

  allocator::stack_allocator_type s;
  std::vector<aligned_int, allocator> v{allocator(s)};

  v.emplace_back(7);
  ASSERT_TRUE(s.owns(&v[0]));

  const std::size_t capacity_1 = v.capacity();
  ASSERT_EQ(1, capacity_1);  //  Test makes this assumption about std::vector.

  v.emplace_back(13);
  ASSERT_TRUE(s.owns(&v[0]));
  const std::size_t capacity_2 = v.capacity();

  for (int x = 0; x < 7; ++x) {
    v.emplace_back(x);
  }
  ASSERT_FALSE(s.owns(&v[0]));

  // We expect some allocations to still be present, because on the second push_back the vector
  // did a copy into a new stack buffer - which precludes releasing the first one.
  const std::size_t non_reclaimed_bytes = (capacity_1 + capacity_2) * sizeof(aligned_int);
  ASSERT_EQ(non_reclaimed_bytes, s.used_bytes());

  v.clear();
  v.shrink_to_fit();
  ASSERT_EQ(non_reclaimed_bytes, s.used_bytes());

  // Put something else into the empty vector.
  v.emplace_back(1974);

  ASSERT_TRUE(s.owns(&v[0]));
  ASSERT_EQ(non_reclaimed_bytes + sizeof(aligned_int), s.used_bytes());

  std::vector v_copied = v;
  ASSERT_TRUE(s.owns(&v_copied[0]));
}

// Test using a linked list.
TEST(StackAllocatorTest, TestList) {
  using allocator = stl_stack_allocator_with_fallback<std::string, sizeof(std::string) * 8>;
  allocator::stack_allocator_type s;

  std::list<std::string, allocator> l{allocator(s)};
  l.insert(l.end(), "hello");

  // std::list will allocate additional bytes for the node itself.
  const std::size_t used_1 = s.used_bytes();
  ASSERT_LT(sizeof(std::string), used_1);
  ASSERT_TRUE(s.owns(std::addressof(*l.begin())));

  // put another entry into the list:
  l.insert(l.end(), "world");
  const std::size_t used_2 = s.used_bytes();
  ASSERT_GT(used_2, used_1);
  ASSERT_LT(sizeof(std::string) * 2, s.used_bytes());
  ASSERT_TRUE(s.owns(std::addressof(*l.rbegin())));

  // Delete the first one from the list (it stays in the stack):
  l.erase(l.begin());
  ASSERT_EQ(used_2, s.used_bytes());

  // Delete the second one:
  l.erase(l.begin());
  ASSERT_EQ(0, l.size());
  ASSERT_EQ(used_1, s.used_bytes());

  // Insert a bunch more strings:
  std::vector<decltype(l)::iterator> stack_owned{};
  for (std::size_t i = 0; i < 10; ++i) {
    auto it = l.insert(l.end(), std::to_string(i));
    if (s.owns(std::addressof(*it))) {
      // save all the parts of the list that are owned by the stack.
      stack_owned.push_back(it);
    }
  }

  // Clean them up in reverse order:
  std::reverse(stack_owned.begin(), stack_owned.end());
  for (const auto& it : stack_owned) {
    l.erase(it);
  }

  // All memory should have been reclaimed:
  ASSERT_EQ(used_1, s.used_bytes());
}

}  // namespace wf
