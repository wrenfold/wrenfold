// Copyright 2023 Gareth Cross
#include "owning_ptr.h"

#include "test_helpers.h"

namespace math {

struct Tracer {
  explicit Tracer(int& counter) : counter_(counter) { ++counter_; }
  ~Tracer() noexcept { --counter_; }

  void DoSomething() const { fmt::print("The count is {}\n", counter_); }

 private:
  int& counter_;
};

TEST(OwningPtrTest, TestConstruct) {
  int tracer_count = 0;
  {
    OwningPtr<Tracer> ptr = MakeOwningPtr<Tracer>(tracer_count);
    ASSERT_TRUE(ptr);
    ASSERT_EQ(1, ptr.GetCount());
    ASSERT_EQ(1, tracer_count);
  }
  ASSERT_EQ(0, tracer_count);

  {
    OwningPtr<Tracer> ptr_1 = MakeOwningPtr<Tracer>(tracer_count);
    OwningPtr<Tracer> ptr_2 = std::move(ptr_1);
    ASSERT_EQ(1, tracer_count);
    ASSERT_TRUE(ptr_2);
    ASSERT_FALSE(ptr_1);
    ASSERT_EQ(1, ptr_2.GetCount());
    ASSERT_EQ(0, ptr_1.GetCount());
  }
  ASSERT_EQ(0, tracer_count);

  // Test making some non-owning ptrs
  {
    OwningPtr<Tracer> ptr = MakeOwningPtr<Tracer>(tracer_count);
    ASSERT_EQ(1, ptr.GetCount());
    ASSERT_EQ(1, tracer_count);

    // Construct a non-owning ptr.
    NonOwningPtr<Tracer> non_owning_1 = ptr;
    ASSERT_TRUE(non_owning_1);
    ASSERT_EQ(2, ptr.GetCount());
    ASSERT_EQ(non_owning_1, ptr);

    // Test move construct:
    NonOwningPtr<Tracer> non_owning_2 = std::move(non_owning_1);
    ASSERT_TRUE(non_owning_2);
    ASSERT_FALSE(non_owning_1);  //  Moved from.
    ASSERT_EQ(2, ptr.GetCount());
    ASSERT_EQ(non_owning_2, ptr);

    // Calling a method on `non_owning_2` should be fine.
    ASSERT_NO_THROW(non_owning_2->DoSomething());

    // Calling a method on non_owning_1 should be a problem:
    ASSERT_THROW(non_owning_1->DoSomething(), AssertionError);

    // Copy construct from another non-owning:
    NonOwningPtr<Tracer> non_owning_3 = non_owning_2;
    ASSERT_TRUE(non_owning_3);
    ASSERT_EQ(3, ptr.GetCount());
    ASSERT_EQ(non_owning_3, non_owning_2);
    ASSERT_EQ(non_owning_3, ptr);
  }
  ASSERT_EQ(0, tracer_count);
}

TEST(OwningPtrTest, TestDestruct) {
  int tracer_count = 0;
  {
    // Construct an owning ptr and immediately let it go out of scope:
    NonOwningPtr<Tracer> non_owning = [&tracer_count]() {
      return MakeOwningPtr<Tracer>(tracer_count);
    }();
    ASSERT_EQ(0, tracer_count);

    // Resulting pointer should be invalid:
    ASSERT_FALSE(non_owning);
    ASSERT_EQ(1, non_owning.GetCount());
    ASSERT_THROW(non_owning->DoSomething(), AssertionError);
  }

  {
    std::optional<NonOwningPtr<Tracer>> non_owner_1{};
    std::optional<NonOwningPtr<Tracer>> non_owner_2{};
    {
      OwningPtr<Tracer> owning = MakeOwningPtr<Tracer>(tracer_count);
      // Create weak copies of it:
      non_owner_1.emplace(owning);
      ASSERT_EQ(2, owning.GetCount());
      non_owner_2.emplace(owning);
      ASSERT_EQ(3, owning.GetCount());
    }
    ASSERT_EQ(0, tracer_count);

    // Now owning has gone out of scope:
    ASSERT_EQ(2, non_owner_1->GetCount());
    ASSERT_FALSE(static_cast<bool>(non_owner_2.value()));

    non_owner_2.reset();
    ASSERT_EQ(1, non_owner_1->GetCount());
  }
  ASSERT_EQ(0, tracer_count);
}

TEST(OwningPtrTest, TestAssignment) {
  int tracer_count = 0;
  {
    OwningPtr<Tracer> owning_1 = MakeOwningPtr<Tracer>(tracer_count);
    OwningPtr<Tracer> owning_2 = MakeOwningPtr<Tracer>(tracer_count);
    ASSERT_EQ(2, tracer_count);

    NonOwningPtr<Tracer> non_owning_1 = owning_1;
    NonOwningPtr<Tracer> non_owning_2 = owning_2;

    std::swap(owning_1, owning_2);
    ASSERT_EQ(2, owning_1.GetCount());
    ASSERT_EQ(2, owning_2.GetCount());
    ASSERT_EQ(owning_1, non_owning_2);
    ASSERT_EQ(owning_2, non_owning_1);
    ASSERT_TRUE(non_owning_1);
    ASSERT_TRUE(non_owning_2);

    // Swap non-owning to match:
    std::swap(non_owning_1, non_owning_2);
    ASSERT_EQ(2, owning_1.GetCount());
    ASSERT_EQ(2, owning_2.GetCount());
    ASSERT_EQ(owning_1, non_owning_1);
    ASSERT_EQ(owning_2, non_owning_2);
    ASSERT_TRUE(non_owning_1);
    ASSERT_TRUE(non_owning_2);

    // Now make non_owning_1 into a moved-from state.
    NonOwningPtr<Tracer> non_owning_3 = std::move(non_owning_1);
    ASSERT_EQ(2, owning_1.GetCount());
    ASSERT_TRUE(non_owning_3);
    ASSERT_FALSE(non_owning_1);
    ASSERT_EQ(0, non_owning_1.GetCount());

    // Now make owning_2 into a moved-from state, thereby destroying its contents:
    owning_1 = std::move(owning_2);

    ASSERT_EQ(owning_1, non_owning_2);
    ASSERT_EQ(1, tracer_count);  // Contents of owning_1 were deleted.
    ASSERT_FALSE(owning_2);      // owning_2 is in a moved-from state.
    ASSERT_EQ(2, owning_1.GetCount());
    ASSERT_EQ(2, non_owning_2.GetCount());

    ASSERT_EQ(non_owning_1, non_owning_3);
    ASSERT_EQ(2, non_owning_3.GetCount());  // Contains non_owning_1 from move assignment above.
    ASSERT_FALSE(non_owning_3);             // But non-valid, since we just deleted owning_1.
  }
  ASSERT_EQ(0, tracer_count);

  // Test moving into self is well-behaved:
  {
    OwningPtr<Tracer> owning = MakeOwningPtr<Tracer>(tracer_count);
    NonOwningPtr<Tracer> non_owning = owning;
    owning = std::move(owning);

    ASSERT_EQ(1, tracer_count);
    ASSERT_TRUE(owning);
    ASSERT_EQ(owning, non_owning);

    non_owning = std::move(non_owning);
    ASSERT_EQ(2, owning.GetCount());
    ASSERT_EQ(owning, non_owning);
  }
  ASSERT_EQ(0, tracer_count);
}

}  // namespace math
