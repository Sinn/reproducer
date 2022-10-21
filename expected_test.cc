// Copyright 2022 The WoW Emu Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "expected.h"

#include <concepts>
#include <initializer_list>
#include <span>
#include <type_traits>

#include "gtest/gtest.h"

namespace {

struct non_default_constructible {
  non_default_constructible() = delete;
};
static_assert(!std::is_default_constructible_v<
              base::expected<non_default_constructible, int>>);

struct default_constructible {
  default_constructible() = default;
};
static_assert(std::is_default_constructible_v<default_constructible>);
static_assert(std::is_default_constructible_v<
              base::expected<void, default_constructible>>);
static_assert(std::is_default_constructible_v<
              base::expected<void, non_default_constructible>>);
static_assert(
    std::is_default_constructible_v<
        base::expected<default_constructible, non_default_constructible>>);
// Copy constructible
struct trivially_copyable {
  trivially_copyable(const trivially_copyable&) = default;
};

struct non_trivially_copyable {
  non_trivially_copyable(const non_trivially_copyable&) = default;
};

struct non_copyable {
  non_copyable(const non_copyable&) = delete;
};

struct construtictible_from {
  constexpr construtictible_from(construtictible_from&&) noexcept {}
};
struct target_type {
  constexpr target_type() = default;
  constexpr target_type(const construtictible_from&) {}
  constexpr target_type(target_type&&) noexcept {}
};
struct convertible {
  constexpr convertible(convertible&&) noexcept {}
  constexpr operator target_type() const { return {}; }
};

struct non_convertible_or_constructible {};

template <typename T, typename E, typename U, typename G>
constexpr void CheckConvertibleAndConstructible() {
  using from = base::expected<U, G>;
  using to = base::expected<T, E>;
  static_assert(!base::expected_internal::convertible_to_expected<T, U, G>);
  static_assert(!base::expected_internal::constructible_from_expected<T, U, G>);
  static_assert(
      !base::expected_internal::unexpected_constructible_from_expected<E, U,
                                                                       G>);
  static_assert(std::is_constructible_v<to, from>);
  static_assert(std::is_constructible_v<to, std::add_const_t<from>>);
  static_assert(std::is_constructible_v<to, std::add_lvalue_reference_t<from>>);
  static_assert(
      std::is_constructible_v<to, std::add_lvalue_reference_t<const from>>);
  static_assert(std::is_constructible_v<to, std::add_rvalue_reference_t<from>>);
  static_assert(
      std::is_constructible_v<to, std::add_rvalue_reference_t<const from>>);
}

template <typename T, typename E, typename U, typename G>
constexpr void CheckNonConvertibleAndConstructible() {
  using from = base::expected<U, G>;
  using to = base::expected<T, E>;
  static_assert(!std::is_convertible_v<to, from>);
  static_assert(!std::is_convertible_v<to, std::add_const_t<from>>);
  static_assert(
      !std::is_convertible_v<to, const std::add_lvalue_reference_t<from>>);
  static_assert(
      !std::is_convertible_v<to, std::add_lvalue_reference_t<const from>>);
  static_assert(
      !std::is_constructible_v<to, std::add_rvalue_reference_t<from>>);
  static_assert(
      !std::is_constructible_v<to, std::add_rvalue_reference_t<const from>>);
}

// Check requirements from lvalue and rvalue references.

struct assign_rhs {};

template <bool is_copy = true, bool is_move = true>
struct assign_lhs {
  assign_lhs() = default;
  assign_lhs(const assign_rhs&) {}
  assign_lhs(assign_rhs&&) {}
  assign_lhs& operator=(const assign_rhs&) { return *this; }
  assign_lhs& operator=(assign_rhs&&) { return *this; }
};

template <>
struct assign_lhs<true, false> {
  assign_lhs() = default;
  assign_lhs(const assign_rhs&) {}
  assign_lhs(assign_rhs&&) = delete;
  assign_lhs(const assign_rhs&&) = delete;
  assign_lhs& operator=(const assign_rhs&) { return *this; }
  assign_lhs& operator=(assign_rhs&&) = delete;
};

template <>
struct assign_lhs<false, true> {
  assign_lhs() = default;
  assign_lhs(const assign_rhs&) = delete;
  assign_lhs(assign_rhs&&) {}
  assign_lhs(const assign_rhs&&) {}
  assign_lhs& operator=(const assign_rhs&) = delete;
  assign_lhs& operator=(assign_rhs&&) { return *this; }
  assign_lhs& operator=(const assign_rhs&&) { return *this; }
};

template <>
struct assign_lhs<false, false> {
  assign_lhs() = default;
  assign_lhs(const assign_rhs&) = delete;
  assign_lhs(assign_rhs&&) = delete;
  assign_lhs& operator=(const assign_rhs&) = delete;
  assign_lhs& operator=(assign_rhs&&) = delete;
};

TEST(ExpectedTraitTest, CheckConstructosFromType) {
  // trivial copy constructor for void if the error is trivially copyable.
  // and if both are trivially copyable.
  static_assert(
      base::expected_internal::expected_supported_type<trivially_copyable>);
  static_assert(std::is_trivially_copy_constructible_v<
                base::expected<trivially_copyable, trivially_copyable>>);

  static_assert(std::is_trivially_copy_constructible_v<
                base::expected<void, trivially_copyable>>);

  static_assert(std::is_copy_constructible_v<
                base::expected<non_trivially_copyable, trivially_copyable>>);
  static_assert(std::is_copy_constructible_v<
                base::expected<trivially_copyable, non_trivially_copyable>>);
  static_assert(std::is_copy_constructible_v<
                base::expected<void, non_trivially_copyable>>);

  // Copy constructor delete for non copyable types/
  static_assert(!std::is_copy_constructible_v<
                base::expected<trivially_copyable, non_copyable>>);
  static_assert(
      !std::is_copy_constructible_v<base::expected<void, non_copyable>>);
  static_assert(!std::is_copy_constructible_v<
                base::expected<non_copyable, trivially_copyable>>);
}

TEST(ExpectedTraitTest, CheckConstructorsFromOtherType) {
  CheckConvertibleAndConstructible<target_type, int, construtictible_from,
                                   int>();
  CheckConvertibleAndConstructible<target_type, int, convertible, int>();
  CheckConvertibleAndConstructible<int, target_type, int,
                                   construtictible_from>();

  CheckConvertibleAndConstructible<int, target_type, int, convertible>();
  CheckConvertibleAndConstructible<void, target_type, void,
                                   construtictible_from>();
  CheckConvertibleAndConstructible<void, target_type, void, convertible>();
  //  Cases when it should not work.
  CheckNonConvertibleAndConstructible<target_type, int,
                                      non_convertible_or_constructible, int>();
  CheckNonConvertibleAndConstructible<void, target_type, void,
                                      non_convertible_or_constructible>();

  CheckNonConvertibleAndConstructible<int, target_type, int,
                                      non_convertible_or_constructible>();

  static_assert(
      std::is_constructible_v<base::expected<int, non_copyable>, int>);
  static_assert(std::is_constructible_v<base::expected<non_copyable, int>,
                                        base::unexpected<int>>);
}

template <typename T, typename U>
concept MoveAssign = requires(T a, U b, const U&& v) {
                       { a = std::move(b) };
                       { a = std::move(v) };
                     };

template <typename T, typename U>
concept CopyAssign = requires(T a, U b, const U& u, const U& v, U& t) {
                       { a = b };
                       { a = u };
                       { a = v };
                       { a = t };
                     };

// Check assignability.
template <typename T, typename E, typename U, bool is_copy, bool is_move>
constexpr void CheckAssignable() {
  using to = base::expected<T, E>;
  // copy assing
  static_assert(CopyAssign<to&, U> == is_copy);
  // move assing
  static_assert(MoveAssign<to&, U&&> == is_move);
}

TEST(ExpectedTraitTest, CheckAssignmentsType) {
  // T, E, U bool bool
  // expected<T,E> = U/const U/ U&/ const U& => #1 bool true
  // expected<T,E> = U&&/const U&&  => #2 bool true
  CheckAssignable<assign_lhs<true, true>, assign_lhs<true, true>, assign_rhs,
                  true, true>();
  CheckAssignable<assign_lhs<false, false>, assign_lhs<false, false>,
                  assign_rhs, false, false>();
  CheckAssignable<assign_lhs<false, true>, assign_lhs<false, true>, assign_rhs,
                  false, true>();
  CheckAssignable<assign_lhs<true, false>, assign_lhs<true, false>, assign_rhs,
                  true, false>();

  CheckAssignable<assign_lhs<true, true>, assign_lhs<true, true>,
                  base::unexpected<assign_rhs>, true, true>();
  CheckAssignable<assign_lhs<true, false>, assign_lhs<true, false>,
                  base::unexpected<assign_rhs>, true, true>();

  CheckAssignable<assign_lhs<false, false>, assign_lhs<false, false>,
                  base::unexpected<assign_rhs>, false, false>();

  CheckAssignable<assign_lhs<false, true>, assign_lhs<false, true>,
                  base::unexpected<assign_rhs>, false, true>();

  CheckAssignable<void, assign_lhs<true, true>, assign_rhs, false, false>();
  CheckAssignable<void, assign_lhs<false, false>, assign_rhs, false, false>();
  CheckAssignable<void, assign_lhs<false, true>, assign_rhs, false, false>();
  CheckAssignable<void, assign_lhs<true, false>, assign_rhs, false, false>();

  CheckAssignable<void, assign_lhs<true, true>, base::unexpected<assign_rhs>,
                  true, true>();
  CheckAssignable<void, assign_lhs<true, false>, base::unexpected<assign_rhs>,
                  true, true>();

  CheckAssignable<void, assign_lhs<false, false>, base::unexpected<assign_rhs>,
                  false, false>();
  CheckAssignable<void, assign_lhs<false, true>, base::unexpected<assign_rhs>,
                  false, true>();
}

template <typename T>
class ExpectedTest : public testing::Test {
 public:
};

struct initializer_list_constructible {
  explicit initializer_list_constructible(int i) : val(i) {}
  initializer_list_constructible(std::initializer_list<int> il)
      : val(*il.begin()) {}

  bool operator==(const initializer_list_constructible&) const = default;

  int val = -1;
};

// Helper expands a std::tuple<A, B> -> ::base::expected<A,B>
template <typename... args>
using ExpectedTypeList =
    testing::Types<::base::expected<std::tuple_element_t<0, args>,
                                    std::tuple_element_t<1, args>>...>;

// List of pairs to test.
using TypeList = ExpectedTypeList<
    std::tuple<int, bool>, std::tuple<void, int>,
    std::tuple<initializer_list_constructible, initializer_list_constructible>>;

// Runtime tests for typed expected.
TYPED_TEST_SUITE(ExpectedTest, TypeList);

TYPED_TEST(ExpectedTest, DefaultConstructible) {
  using expected_t = TypeParam;
  if constexpr (std::is_default_constructible_v<expected_t>) {
    expected_t ex;

    EXPECT_TRUE(ex);
    EXPECT_TRUE(ex.has_value());

    // Default constructed |T| should be the value.
    if constexpr (!std::is_void_v<typename TypeParam::value_type>) {
      typename TypeParam::value_type defaulted_value{};
      EXPECT_EQ(ex.value(), defaulted_value);
    }
  }
}

// Type dependant value initializer.
// Indirection needed so we can initialize T from arbitrary values through
// specialization.
template <typename T, typename... Args>
T MakeValueFrom(Args&&... args) {
  return T(std::forward<Args>(args)...);
}

TYPED_TEST(ExpectedTest, CopyConstructible) {
  using expected_t = TypeParam;
  using value_t = typename expected_t::value_type;
  using error_t = typename expected_t::error_type;

  if constexpr (std::is_copy_constructible_v<expected_t> &&
                !std::is_void_v<value_t>) {
    value_t val = MakeValueFrom<value_t>(1);
    expected_t ex(val);
    expected_t ex_2(ex);

    ASSERT_TRUE(ex.has_value());
    ASSERT_TRUE(ex_2.has_value());

    if constexpr (std::equality_comparable<value_t>) {
      EXPECT_EQ(ex.value(), ex_2.value());
    }
  }

  if constexpr (std::is_copy_constructible_v<error_t>) {
    error_t err = MakeValueFrom<error_t>(1);
    expected_t ex(base::unexpected{err});
    expected_t ex_2(ex);

    ASSERT_FALSE(ex.has_value());
    ASSERT_FALSE(ex_2.has_value());

    if constexpr (std::equality_comparable<error_t>) {
      EXPECT_EQ(ex.error(), ex_2.error());
    }
  }
}

TYPED_TEST(ExpectedTest, MoveConstructible) {
  using expected_t = TypeParam;
  using value_t = typename expected_t::value_type;
  using error_t = typename expected_t::error_type;

  if constexpr (std::is_move_constructible_v<expected_t>) {
    if constexpr (!std::is_void_v<value_t>) {
      value_t val = MakeValueFrom<value_t>(1);
      expected_t ex(val);
      expected_t ex_1(val);
      expected_t ex_2(std::move(ex_1));

      ASSERT_TRUE(ex.has_value());
      ASSERT_TRUE(ex_2.has_value());

      if constexpr (std::equality_comparable<value_t>) {
        EXPECT_EQ(ex.value(), ex_2.value());
      }
    } else {
      expected_t ex;
      expected_t ex_1(std::move(ex));
      ASSERT_TRUE(ex.has_value());
      ASSERT_TRUE(ex_1.has_value());
    }
  }

  if constexpr (std::is_move_constructible_v<error_t>) {
    error_t err = MakeValueFrom<error_t>(1);
    expected_t ex(base::unexpected{err});
    expected_t ex_1(ex);
    expected_t ex_2(std::move(ex_1));

    ASSERT_FALSE(ex.has_value());
    ASSERT_FALSE(ex_2.has_value());

    if constexpr (std::equality_comparable<error_t>) {
      EXPECT_EQ(ex.error(), ex_2.error());
    }
  }
}

TYPED_TEST(ExpectedTest, ValueConstructible) {
  using expected_t = TypeParam;
  using value_t = typename expected_t::value_type;
  using error_t = typename expected_t::error_type;

  if constexpr (!std::is_void_v<value_t>) {
    if constexpr (std::is_constructible_v<expected_t, value_t&>) {
      value_t val = MakeValueFrom<value_t>(1);
      expected_t ex(val);

      ASSERT_TRUE(ex.has_value());

      if constexpr (std::equality_comparable<value_t>) {
        EXPECT_EQ(ex.value(), val);
      }
    }

    if constexpr (std::is_constructible_v<expected_t, value_t&&>) {
      value_t val = MakeValueFrom<value_t>(1);
      expected_t ex(std::move(val));
      ASSERT_TRUE(ex.has_value());

      if constexpr (std::equality_comparable<value_t>) {
        EXPECT_EQ(ex.value(), val);
      }
    }
  }

  if (std::is_constructible_v<expected_t, base::unexpected<error_t>&>) {
    error_t err = MakeValueFrom<error_t>(1);
    base::unexpected unex{err};
    expected_t ex(unex);
    ASSERT_FALSE(ex.has_value());

    if constexpr (std::equality_comparable<error_t>) {
      EXPECT_EQ(ex.error(), err);
    }
  }

  if (std::is_constructible_v<expected_t, base::unexpected<error_t>&&>) {
    error_t err = MakeValueFrom<error_t>(1);
    base::unexpected unex{err};
    expected_t ex(std::move(unex));
    ASSERT_FALSE(ex.has_value());

    if constexpr (std::equality_comparable<error_t>) {
      EXPECT_EQ(ex.error(), err);
    }
  }
}

TYPED_TEST(ExpectedTest, TaggedConstruction) {
  using expected_t = TypeParam;
  using value_t = typename expected_t::value_type;
  using error_t = typename expected_t::error_type;

  if constexpr (!std::is_void_v<value_t>) {
    value_t val = MakeValueFrom<value_t>(1);
    expected_t ex(base::in_place_t{}, 1);

    ASSERT_TRUE(ex.has_value());
    if constexpr (std::equality_comparable<error_t>) {
      EXPECT_EQ(*ex, val);
    }
  } else {
    expected_t ex(base::in_place_t{});
    ASSERT_TRUE(ex.has_value());
  }

  error_t err = MakeValueFrom<error_t>(1);
  expected_t ex(base::unexpect, 1);
  ASSERT_FALSE(ex.has_value());
  if constexpr (std::equality_comparable<error_t>) {
    EXPECT_EQ(ex.error(), err);
  }
}

TYPED_TEST(ExpectedTest, TaggedConstructionWithInitializerList) {
  using expected_t = TypeParam;
  using value_t = typename expected_t::value_type;
  using error_t = typename expected_t::error_type;

  if constexpr (!std::is_void_v<value_t> &&
                std::is_constructible_v<value_t, std::initializer_list<int>>) {
    value_t val = MakeValueFrom<value_t>(std::initializer_list<int>{1});
    expected_t ex(base::in_place_t{}, std::initializer_list<int>{1});

    ASSERT_TRUE(ex.has_value());
    if constexpr (std::equality_comparable<error_t>) {
      EXPECT_EQ(*ex, val);
    }
  }

  if constexpr (std::is_constructible_v<error_t, std::initializer_list<int>>) {
    error_t err = MakeValueFrom<error_t>(std::initializer_list<int>{1});
    expected_t ex(base::unexpect, std::initializer_list<int>{1});
    ASSERT_FALSE(ex.has_value());
    if constexpr (std::equality_comparable<error_t>) {
      EXPECT_EQ(ex.error(), err);
    }
  }
}

constexpr int kSentinel = 1234567;

// Destructible class with verifyable side effect.
struct destructible_t {
  destructible_t() = default;
  destructible_t(int& i) : ptr(&i) {}
  destructible_t(const destructible_t&) = default;
  ~destructible_t() {
    if (ptr) {
      *ptr = kSentinel;
    }
  }

  int* ptr = nullptr;
};

struct assignable_t : public destructible_t {
  assignable_t(std::initializer_list<int> a, int& sentinel) noexcept
      : destructible_t(sentinel), val(*a.begin()) {}
  assignable_t(int a, int& sentinel) noexcept
      : destructible_t(sentinel), val(a) {}

  assignable_t(const assignable_t&) noexcept = default;
  assignable_t(assignable_t&& other) noexcept { *this = std::move(other); }

  assignable_t& operator=(const assignable_t& a) noexcept {
    val = a.val;
    return *this;
  }

  assignable_t& operator=(assignable_t&& other) noexcept {
    ptr = other.ptr;
    val = other.val;
    other.ptr = nullptr;
    return *this;
  }

  int val;
};

template <typename T>
class ExpectedAssignableTest : public testing::Test {};

// T values to try, for both specializations.
using Types = testing::Types<assignable_t, void>;

TYPED_TEST_SUITE(ExpectedAssignableTest, Types);

TYPED_TEST(ExpectedAssignableTest, AssignmentValueFromValue) {
  if constexpr (std::is_void_v<TypeParam>) {
    GTEST_SKIP() << " void type";
  } else {
    int sentinel = 0;

    assignable_t b(1, sentinel);
    // Value to value, should not destruct assignable.
    base::expected<TypeParam, assignable_t> ex(base::in_place_t{}, 2, sentinel);

    ASSERT_TRUE(ex.has_value());
    ASSERT_EQ(ex->val, 2);

    ex = b;

    ASSERT_TRUE(ex.has_value());
    EXPECT_EQ(ex->val, 1);
    EXPECT_EQ(sentinel, 0);
  }
}

TYPED_TEST(ExpectedAssignableTest, AssignmentValueFromUnexpected) {
  int sentinel = 0;
  assignable_t b(1, sentinel);
  // Value to value, should not destruct assignable.
  if constexpr (!std::is_void_v<TypeParam>) {
    base::expected<TypeParam, assignable_t> ex(base::in_place_t{}, 2, sentinel);
    base::unexpected<assignable_t> unex(b);

    ASSERT_TRUE(ex.has_value());
    ASSERT_EQ(ex->val, 2);

    ex = unex;

    ASSERT_FALSE(ex.has_value());
    EXPECT_EQ(ex.error().val, 1);
    // Because we swapped from ex -> unex, the T value should have been
    // destroyed by now.
    EXPECT_EQ(sentinel, kSentinel);
  } else {
    base::expected<TypeParam, assignable_t> ex;
    base::unexpected<assignable_t> unex(b);

    ASSERT_TRUE(ex.has_value());

    ex = unex;

    ASSERT_FALSE(ex.has_value());
    EXPECT_EQ(ex.error().val, 1);
    // Because we swapped from ex -> unex, the T value should have been
    // destroyed by now.
    EXPECT_EQ(sentinel, 0);
  }
}

TYPED_TEST(ExpectedAssignableTest, AssignmentUnexpectedFromValue) {
  if constexpr (std::is_void_v<TypeParam>) {
    GTEST_SKIP() << "void type";
  } else {
    int sentinel = 0;

    assignable_t b(1, sentinel);
    // Value to value, should not destruct assignable.
    base::expected<TypeParam, assignable_t> ex(base::unexpect, 2, sentinel);

    ASSERT_FALSE(ex.has_value());
    ASSERT_EQ(ex.error().val, 2);

    ex = b;

    ASSERT_TRUE(ex.has_value());
    EXPECT_EQ(ex->val, 1);
    // Because we swapped from unex -> ex, the E error should have been
    // destroyed by now.
    EXPECT_EQ(sentinel, kSentinel);
  }
}

TEST(ExpectedTest, Emplace) {
  {
    int sentinel = 0;
    // Value to value, should not destruct assignable.
    base::expected<assignable_t, assignable_t> ex(base::unexpect, 2, sentinel);
    ASSERT_FALSE(ex.has_value());
    ASSERT_EQ(ex.error().val, 2);

    ex.emplace(1, sentinel);

    ASSERT_TRUE(ex.has_value());
    EXPECT_EQ(ex->val, 1);
    // the unexpected must have been destroyed.
    EXPECT_EQ(sentinel, kSentinel);
  }
  {
    int sentinel = 0;
    // Value to value, should not destruct assignable.
    base::expected<assignable_t, assignable_t> ex(base::in_place_t{}, 2,
                                                  sentinel);
    ASSERT_TRUE(ex.has_value());
    ASSERT_EQ(ex->val, 2);

    ex.emplace(1, sentinel);

    ASSERT_TRUE(ex.has_value());
    EXPECT_EQ(ex->val, 1);
    EXPECT_EQ(sentinel, kSentinel);
  }
  {
    int sentinel = 0;
    // Value to value, should not destruct assignable.
    base::expected<void, assignable_t> ex(base::unexpect, 2, sentinel);
    ASSERT_FALSE(ex.has_value());
    ASSERT_EQ(ex.error().val, 2);

    ex.emplace();

    ASSERT_TRUE(ex.has_value());
    // the unexpected must have been destroyed.
    EXPECT_EQ(sentinel, kSentinel);
  }
}

TEST(ExpectedTest, EmplaceFromInitializerList) {
  {
    int sentinel = 0;
    // Value to value, should not destruct assignable.
    base::expected<assignable_t, assignable_t> ex(base::unexpect, 2, sentinel);
    ASSERT_FALSE(ex.has_value());
    ASSERT_EQ(ex.error().val, 2);

    ex.emplace({6, 2, 3, 4}, sentinel);

    ASSERT_TRUE(ex.has_value());
    EXPECT_EQ(ex->val, 6);
    // the unexpected must have been destroyed.
    EXPECT_EQ(sentinel, kSentinel);
  }
  {
    int sentinel = 0;
    // Value to value, should not destruct assignable.
    base::expected<assignable_t, assignable_t> ex(base::in_place_t{}, 2,
                                                  sentinel);
    ASSERT_TRUE(ex.has_value());
    ASSERT_EQ(ex->val, 2);

    ex.emplace({3, 4}, sentinel);

    ASSERT_TRUE(ex.has_value());
    EXPECT_EQ(ex->val, 3);
    EXPECT_EQ(sentinel, kSentinel);
  }
  {
    int sentinel = 0;
    // Value to value, should not destruct assignable.
    base::expected<void, assignable_t> ex(base::unexpect, 2, sentinel);
    ASSERT_FALSE(ex.has_value());
    ASSERT_EQ(ex.error().val, 2);

    ex.emplace();

    ASSERT_TRUE(ex.has_value());
    // the unexpected must have been destroyed.
    EXPECT_EQ(sentinel, kSentinel);
  }
}

TEST(ExpectedTest, Swap) {
  // base::expected<T,E>
  {
    int sentinel = 0;
    int sentinel_2 = 0;
    base::expected<assignable_t, bool> ex(base::in_place_t{}, 1, sentinel);
    base::expected<assignable_t, bool> ex_2(base::in_place_t{}, 2, sentinel_2);

    ASSERT_TRUE(ex);
    ASSERT_TRUE(ex_2);

    ex.swap(ex_2);

    ASSERT_TRUE(ex);
    ASSERT_TRUE(ex_2);
    EXPECT_EQ(ex->val, 2);
    EXPECT_EQ(ex_2->val, 1);
    EXPECT_EQ(sentinel, 0);
    EXPECT_EQ(sentinel_2, 0);
  }

  {
    int sentinel = 0;
    int sentinel_2 = 0;
    base::expected<bool, assignable_t> ex(base::unexpect, 1, sentinel);
    base::expected<bool, assignable_t> ex_2(base::unexpect, 2, sentinel_2);

    ASSERT_FALSE(ex);
    ASSERT_FALSE(ex_2);

    ex.swap(ex_2);

    ASSERT_FALSE(ex);
    ASSERT_FALSE(ex_2);

    EXPECT_EQ(ex_2.error().val, 1);
    EXPECT_EQ(ex.error().val, 2);
    EXPECT_EQ(sentinel, 0);
    EXPECT_EQ(sentinel_2, 0);
  }

  {
    int sentinel = 0;
    int sentinel_2 = 0;
    base::expected<assignable_t, assignable_t> ex(base::unexpect, 1, sentinel);
    base::expected<assignable_t, assignable_t> ex_2(base::in_place_t{}, 2,
                                                    sentinel_2);

    ASSERT_FALSE(ex);
    ASSERT_TRUE(ex_2);

    ex.swap(ex_2);

    ASSERT_TRUE(ex);
    ASSERT_FALSE(ex_2);
    EXPECT_EQ(ex->val, 2);
    EXPECT_EQ(ex_2.error().val, 1);
    EXPECT_EQ(sentinel, 0);
    EXPECT_EQ(sentinel_2, 0);

    ex.swap(ex_2);
    ASSERT_TRUE(ex_2);
    ASSERT_FALSE(ex);
    EXPECT_EQ(ex.error().val, 1);
    EXPECT_EQ(ex_2->val, 2);
    EXPECT_EQ(sentinel, 0);
    EXPECT_EQ(sentinel_2, 0);
  }

  // base::expected<void, E>
  {
    base::expected<void, bool> ex;
    base::expected<void, bool> ex_2;
    ASSERT_TRUE(ex);
    ASSERT_TRUE(ex_2);

    ex.swap(ex_2);

    EXPECT_TRUE(ex);
    EXPECT_TRUE(ex_2);
  }

  {
    int sentinel = 0;
    int sentinel_2 = 0;
    base::expected<void, assignable_t> ex(base::unexpect, 1, sentinel);
    base::expected<void, assignable_t> ex_2(base::unexpect, 2, sentinel_2);

    ASSERT_FALSE(ex);
    ASSERT_FALSE(ex_2);

    ex.swap(ex_2);

    ASSERT_FALSE(ex);
    ASSERT_FALSE(ex_2);

    EXPECT_EQ(ex_2.error().val, 1);
    EXPECT_EQ(ex.error().val, 2);
    EXPECT_EQ(sentinel, 0);
    EXPECT_EQ(sentinel_2, 0);
  }

  {
    int sentinel = 0;
    base::expected<void, assignable_t> ex(base::unexpect, 1, sentinel);
    base::expected<void, assignable_t> ex_2;

    ASSERT_FALSE(ex);
    ASSERT_TRUE(ex_2);

    ex.swap(ex_2);

    EXPECT_TRUE(ex);
    ASSERT_FALSE(ex_2);
    EXPECT_EQ(ex_2.error().val, 1);
    EXPECT_EQ(sentinel, 0);

    ex.swap(ex_2);
    EXPECT_TRUE(ex_2);
    ASSERT_FALSE(ex);
    EXPECT_EQ(ex.error().val, 1);
    EXPECT_EQ(sentinel, 0);
  }
}

auto check_compare = [](auto& ex_1, auto& ex_2, auto& ex_3, auto& unex_1,
                        auto& unex_2, auto& unex_3, auto& unex_4,
                        auto& unex_5) {
  EXPECT_EQ(ex_1, ex_1);
  EXPECT_EQ(ex_1, ex_2);
  EXPECT_EQ(ex_2, ex_1);
  EXPECT_EQ(ex_2, ex_2);

  // ex_3 is mean to represent another expected with a different value.
  using expected_t = std::decay_t<decltype(ex_1)>;
  if constexpr (!std::is_void_v<typename expected_t::value_type>) {
    EXPECT_EQ(ex_3, ex_3);
    EXPECT_NE(ex_2, ex_3);
    EXPECT_NE(ex_3, ex_2);

    EXPECT_EQ(ex_3, *ex_3);
    EXPECT_EQ(*ex_3, ex_3);
    EXPECT_NE(ex_3, *ex_1);
    EXPECT_NE(*ex_1, ex_3);
  }

  EXPECT_NE(ex_1, unex_1);
  EXPECT_NE(unex_1, ex_1);
  EXPECT_NE(ex_1, unex_2);
  EXPECT_NE(unex_2, ex_1);
  EXPECT_NE(ex_1, unex_3);
  EXPECT_NE(unex_3, ex_1);

  EXPECT_EQ(unex_1, unex_1);
  EXPECT_EQ(unex_1, unex_2);
  EXPECT_EQ(unex_2, unex_1);
  EXPECT_EQ(unex_2, unex_2);
  EXPECT_EQ(unex_3, unex_3);

  EXPECT_NE(unex_1, unex_3);
  EXPECT_NE(unex_3, unex_1);
  EXPECT_NE(unex_2, unex_3);
  EXPECT_NE(unex_3, unex_2);

  // expected == unexpected
  EXPECT_EQ(unex_1, unex_4);
  EXPECT_EQ(unex_4, unex_1);
  EXPECT_EQ(unex_5, unex_3);
  EXPECT_EQ(unex_3, unex_5);

  EXPECT_NE(ex_1, unex_4);
  EXPECT_NE(unex_4, ex_1);
  EXPECT_NE(ex_1, unex_5);
  EXPECT_NE(unex_5, ex_1);
  EXPECT_NE(unex_4, unex_3);
  EXPECT_NE(unex_3, unex_4);
  EXPECT_NE(unex_5, unex_2);
  EXPECT_NE(unex_2, unex_5);
};

TEST(ExpectedTest, Equality) {
  // expected<void, E>
  {
    base::expected<void, bool> ex_1;
    base::expected<void, bool> ex_2;
    base::expected<void, bool> ex_3;
    base::expected<void, bool> unex_1(base::unexpect, false);
    base::expected<void, bool> unex_2(base::unexpect, false);
    base::expected<void, bool> unex_3(base::unexpect, true);
    base::unexpected<bool> unex_4(false);
    base::unexpected<bool> unex_5(true);
    check_compare(ex_1, ex_2, ex_3, unex_1, unex_2, unex_3, unex_4, unex_5);
  }

  {
    base::expected<int, bool> ex_1(1);
    base::expected<int, bool> ex_2(1);
    base::expected<int, bool> ex_3(2);
    base::expected<int, bool> unex_1(base::unexpect, false);
    base::expected<int, bool> unex_2(base::unexpect, false);
    base::expected<int, bool> unex_3(base::unexpect, true);
    base::unexpected<bool> unex_4(false);
    base::unexpected<bool> unex_5(true);
    check_compare(ex_1, ex_2, ex_3, unex_1, unex_2, unex_3, unex_4, unex_5);
  }
}

TEST(ExpectedTest, ValueOr) {
  base::expected<int, bool> ok(1234);
  EXPECT_EQ(ok.value_or(4321), 1234);

  base::expected<int, bool> not_ok(base::unexpect, false);
  EXPECT_EQ(not_ok.value_or(4321), 4321);
}

TEST(ExpectedTest, Destructor) {
  {
    int sentinel = 0;
    {
      base::expected<destructible_t, destructible_t> ex(base::in_place_t{},
                                                        sentinel);
    }
    EXPECT_EQ(sentinel, kSentinel);
  }

  {
    int sentinel = 0;
    {
      base::expected<destructible_t, destructible_t> ex(base::unexpect_t{},
                                                        sentinel);
    }
    EXPECT_EQ(sentinel, kSentinel);
  }

  {
    int sentinel = 0;
    { base::expected<void, destructible_t> ex(base::unexpect_t{}, sentinel); }
    EXPECT_EQ(sentinel, kSentinel);
  }
}

TEST(UnexpectedTest, ConstructorFromValue) {
  base::unexpected<bool> unex(false);
  EXPECT_FALSE(unex.error());
}

TEST(UnexpectedTest, Assignment) {
  base::unexpected<bool> unex(false);
  base::unexpected<bool> unex_2(true);
  ASSERT_FALSE(unex.error());

  unex = unex_2;

  EXPECT_TRUE(unex.error());
  EXPECT_TRUE(unex_2.error());
}

TEST(UnexpectedTest, MoveAssignment) {
  int sentinel = 0;
  int sentinel_2 = 0;
  base::unexpected<assignable_t> unex(base::in_place_t{}, 1, sentinel);
  base::unexpected<assignable_t> unex_2(base::in_place_t{}, 2, sentinel_2);

  ASSERT_EQ(unex.error().val, 1);
  ASSERT_EQ(unex_2.error().val, 2);

  unex = std::move(unex_2);

  EXPECT_EQ(unex.error().val, 2);
  EXPECT_EQ(unex_2.error().ptr, nullptr);
  EXPECT_EQ(sentinel, 0);
  EXPECT_EQ(sentinel_2, 0);
}

TEST(UnexpectedTest, Swap) {
  int sentinel = 0;
  int sentinel_2 = 0;
  base::unexpected<assignable_t> unex(base::in_place_t{}, 1, sentinel);
  base::unexpected<assignable_t> unex_2(base::in_place_t{}, 2, sentinel_2);

  ASSERT_EQ(unex.error().val, 1);
  ASSERT_EQ(unex_2.error().val, 2);

  unex.swap(unex_2);

  EXPECT_EQ(unex.error().val, 2);
  EXPECT_EQ(unex_2.error().val, 1);
  EXPECT_EQ(sentinel, 0);
  EXPECT_EQ(sentinel_2, 0);

  unex.swap(unex_2);

  EXPECT_EQ(unex.error().val, 1);
  EXPECT_EQ(unex_2.error().val, 2);
  EXPECT_EQ(sentinel, 0);
  EXPECT_EQ(sentinel_2, 0);
}

TEST(UnexpectedTest, Equality) {
  base::unexpected<int> unex_1(1);
  base::unexpected<int> unex_2(1);
  base::unexpected<int> unex_3(2);

  EXPECT_EQ(unex_1, unex_1);
  EXPECT_EQ(unex_1, unex_2);
  EXPECT_EQ(unex_2, unex_1);
  EXPECT_EQ(unex_2, unex_2);
  EXPECT_NE(unex_1, unex_3);
  EXPECT_NE(unex_3, unex_1);
}

}  // namespace
