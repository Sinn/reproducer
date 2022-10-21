// Copyright 2022 The WoW Emu Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include <initializer_list>
#include <memory>
#include <new>
#include <type_traits>
#include <utility>

namespace base {

using in_place_t = std::in_place_t;

// inplace construction tag.
struct unexpect_t {
  explicit unexpect_t() = default;
};

template <typename T, typename E>
class expected;

template <typename E>
class unexpected;

namespace expected_internal {

template <typename T>
inline constexpr bool is_non_trivially_copy_constructible =
    std::is_copy_constructible_v<T> && !std::is_trivially_copy_constructible_v<T>;

template <typename T>
inline constexpr bool is_non_trivially_move_constructible =
    std::is_move_constructible_v<T> && !std::is_trivially_move_constructible_v<T>;

template <typename T>
concept void_type = std::is_void_v<T>;

template <typename T, typename U>
concept is_convertible = requires {
                           std::is_convertible_v<T, U>;
                           std::is_convertible_v<T, const U>;
                           std::is_convertible_v<T, U&>;
                           std::is_convertible_v<T, const U&>;
                         };

template <typename T, typename U>
concept is_constructible = requires {
                             std::is_convertible_v<T, U>;
                             std::is_convertible_v<T, const U>;
                             std::is_convertible_v<T, U&>;
                             std::is_convertible_v<T, const U&>;
                           };

template <typename T, template <typename...> typename U>
struct is_template_specialization_t : std::false_type {};

template <typename... Args, template <typename> typename U>
struct is_template_specialization_t<U<Args...>, U> : std::true_type {};

template <typename T, template <typename...> typename U>
concept is_template_specialization = is_template_specialization_t<T, U>::value;

// Wheter operator= is defined for expected<T,E>
template <typename T, typename E>
concept is_copy_assignable = (std::is_void_v<T> || std::is_copy_assignable_v<T>) &&
                             (std::is_void_v<T> || std::is_copy_constructible_v<T>) &&
                             (std::is_copy_assignable_v<E> && std::is_copy_constructible_v<E>) &&
                             (std::is_void_v<T> || std::is_nothrow_move_constructible_v<T> ||
                              std::is_nothrow_move_constructible_v<E>);

// Wheter operator= is defined for expected<T,E>
template <typename T, typename E>
concept is_move_assignable = (std::is_void_v<T> || std::is_move_assignable_v<T>) &&
                             (std::is_void_v<T> || std::is_move_constructible_v<T>) &&
                             (std::is_move_assignable_v<E> && std::is_move_constructible_v<E>) &&
                             (std::is_void_v<T> || std::is_nothrow_move_constructible_v<T> ||
                              std::is_nothrow_move_constructible_v<E>);

// Whether swap is defined for expected<T,E>
template <typename T, typename E>
concept is_swappable =
    (std::is_void_v<T> || std::is_swappable_v<T>) && std::is_swappable_v<E> &&
    (std::is_void_v<T> || std::is_move_constructible_v<T>) && std::is_move_constructible_v<E> &&
    (std::is_void_v<T> || std::is_nothrow_move_constructible_v<T> ||
     std::is_nothrow_move_constructible_v<E>);

template <typename E>
concept unexpected_supported_type =
    std::is_object_v<E> && !std::is_array_v<E> && std::is_same_v<E, std::remove_cv_t<E>>;

template <typename U>
concept unexpected_type = expected_internal::is_template_specialization<U, unexpected>;

template <typename U>
concept expected_type = expected_internal::is_template_specialization<U, expected>;

template <typename T, typename U, typename G>
concept constructible_from_expected = std::is_constructible_v<T, expected<U, G>> ||
                                      std::is_constructible_v<T, const expected<U, G>> ||
                                      std::is_constructible_v<T, expected<U, G>&> ||
                                      std::is_constructible_v<T, const expected<U, G>&>;

template <typename E, typename U, typename G>
concept unexpected_constructible_from_expected =
    std::is_constructible_v<unexpected<E>, expected<U, G>> ||
    std::is_constructible_v<unexpected<E>, const expected<U, G>> ||
    std::is_constructible_v<unexpected<E>, expected<U, G>&> ||
    std::is_constructible_v<unexpected<E>, const expected<U, G>&>;

template <typename T, typename U, typename G>
concept convertible_to_expected =
    std::is_convertible_v<expected<U, G>, T> || std::is_convertible_v<const expected<U, G>, T> ||
    std::is_convertible_v<expected<U, G>&, T> || std::is_convertible_v<const expected<U, G>&, T>;

template <typename T>
concept expected_supported_type =
    !std::is_reference_v<T> && !std::is_function_v<T> &&
    !std::is_same_v<std::remove_cv_t<T>, in_place_t> &&
    !std::is_same_v<std::remove_cv_t<T>, unexpect_t> && !unexpected_type<T> && !expected_type<T>;

// Whether expected can be assigned from an arbitrary type U that is not an
// expected<V>.
template <typename T, typename E, typename U>
concept expected_assignable_from =
    !unexpected_type<std::remove_cvref_t<U>> && std::is_constructible_v<T, U> &&
    std::is_assignable_v<T&, U> &&
    (std::is_nothrow_constructible_v<T, U> || std::is_nothrow_move_constructible_v<T> ||
     std::is_nothrow_move_constructible_v<E>);

template <typename T, typename E, typename GF>
concept expected_assignable_from_unexpected = std::is_constructible_v<E, GF> &&
                                              std::is_assignable_v<E&, GF> &&
                                              (std::is_nothrow_constructible_v<E, GF> ||
                                               std::is_nothrow_move_constructible_v<T> ||
                                               std::is_nothrow_move_constructible_v<E>);

template <typename T, typename E, typename U, typename G>
concept explicit_conversion = !std::is_convertible_v<U, T> || !std::is_convertible_v<G, U>;

template <typename U, typename... Types>
concept any_type = (std::is_same_v<U, std::remove_cvref_t<Types>> || ...);

// RAII to reset on destruction. Avoid doing explicit exception handling,
// instead let the exception resolution destroy the value_guard, such that
// it resets the value to a previous value.
template <typename T>
  requires(std::is_nothrow_move_constructible_v<T>)
struct value_guard {
  explicit constexpr value_guard(T& v) : value_(std::addressof(v)), reset_to_(std::move(v)) {
    std::destroy_at(value_);
  }
  constexpr value_guard(const value_guard&) = delete;
  constexpr value_guard(value_guard&&) = delete;
  constexpr ~value_guard() {
    if (value_) [[unlikely]] {
      std::construct_at(value_, std::move(reset_to_));
    }
  }

  T&& release() {
    value_ = nullptr;
    return std::move(reset_to_);
  }

  // Address of the value to guard.
  T* value_;
  // Value to reset to.
  T reset_to_;
};

template <typename T, typename U, typename... Args>
constexpr void replace_value(T& new_val, U& old_val, Args&&... args) {
  if constexpr (std::is_nothrow_constructible_v<T, U>) {
    std::destroy_at(std::addressof(old_val));
    std::construct_at(std::addressof(new_val), std::forward<Args>(args)...);
  } else if (std::is_nothrow_move_constructible_v<T>) {
    T val(std::forward<Args>(args)...);
    std::destroy_at(std::addressof(old_val));
    std::construct_at(std::addressof(new_val), std::move(val));
  } else {
    // Use the value_guard RAII to have the exception place old val into the
    // previous spot.
    value_guard<U> old_guard(old_val);
    std::construct_at(std::addressof(new_val), std::forward<Args...>(args)...);
    old_guard.release();
  }
}

template <typename T, typename E>
union storage {
  constexpr storage() noexcept
    requires(std::is_default_constructible_v<T>)
      : value() {}

  constexpr ~storage() noexcept = default;

  constexpr ~storage() noexcept
    requires(!std::is_trivially_destructible_v<T> || !std::is_trivially_destructible_v<E>)
  {}

  template <typename... Args>
  constexpr storage(in_place_t, Args&&... args) : value(std::forward<Args>(args)...) {}

  template <typename... Args>
  constexpr storage(unexpect_t, Args&&... args) : unexpected(std::forward<Args>(args)...) {}

  T value;
  E unexpected;
};

}  // namespace expected_internal

// Used as a constant to be passed to constructors.
inline constexpr unexpect_t unexpect{};

// Represents an unexpected value. E.g. an error.
template <typename E>
  requires(expected_internal::unexpected_supported_type<E> &&
           !expected_internal::unexpected_type<E>)
class unexpected<E> {
 public:
  constexpr unexpected(const unexpected&) = default;
  constexpr unexpected(unexpected&&) noexcept = default;

  template <typename Err = E>
    requires(!std::is_same_v<std::remove_cvref_t<Err>, unexpected> &&
             !std::is_same_v<std::remove_cvref_t<Err>, in_place_t> &&
             std::is_constructible_v<E, Err>)
  constexpr explicit unexpected(Err&& e) : e_(std::forward<Err>(e)) {}

  template <class... Args>
    requires(std::is_constructible_v<E, Args...>)
  constexpr explicit unexpected(in_place_t, Args&&... args) : e_(std::forward<Args>(args)...) {}

  template <typename U, class... Args>
    requires(std::is_constructible_v<E, std::initializer_list<U>&, Args...>)
  constexpr explicit unexpected(in_place_t, std::initializer_list<U> il, Args&&... args)
      : e_(il, std::forward<Args>(args)...) {}

  constexpr unexpected& operator=(const unexpected&) = default;
  constexpr unexpected& operator=(unexpected&&) = default;

  constexpr E& error() & { return e_; }
  constexpr const E& error() const& { return e_; }
  constexpr E&& error() && { return std::move(e_); }
  constexpr E&& error() const&& { return std::move(e_); }

  constexpr void swap(unexpected& other) noexcept(std::is_nothrow_swappable_v<E>) {
    using std::swap;
    swap(e_, other.e_);
  }

  friend constexpr void swap(unexpected& x, unexpected& y) noexcept(std::is_nothrow_swappable_v<E>)
    requires std::is_swappable_v<E>
  {
    x.swap(y);
  }

  template <class E2>
  friend constexpr bool operator==(const unexpected& lhs, const base::unexpected<E2>& rhs) noexcept(
      noexcept(bool(lhs.error() == rhs.error()))) {
    return bool(lhs.error() == rhs.error());
  }

 private:
  E e_;
};

template <class E>
unexpected(E) -> unexpected<E>;

// Polyfill for std++23 expected. Someday.
template <typename T, typename E>
  requires(!std::is_void_v<T> && expected_internal::expected_supported_type<T> &&
           expected_internal::unexpected_supported_type<E>)
class expected<T, E> {
  static_assert(!std::is_void_v<T>);

  template <typename V>
  using ref_t = std::add_lvalue_reference_t<V>;

  template <typename U, typename G>
  static constexpr bool is_explicit = !expected_internal::explicit_conversion<T, E, U, G>;

  // This checks that T and unexpected<U> are not constructible
  // from expected<U, G>.
  template <typename U, typename G>
  static constexpr bool convertible_value =
      !expected_internal::convertible_to_expected<T, U, G> &&
      !expected_internal::constructible_from_expected<T, U, G> &&
      !expected_internal::unexpected_constructible_from_expected<E, U, G>;

 public:
  using value_type = T;
  using error_type = E;
  using unexpected_type = unexpected<E>;

  template <typename U>
  using rebind = expected<U, error_type>;

  constexpr expected() noexcept(std::is_nothrow_default_constructible_v<T>)
    requires std::is_default_constructible_v<T>
  = default;

  constexpr expected(const expected& other) noexcept(
      std::is_nothrow_copy_constructible_v<T>&& std::is_nothrow_copy_constructible_v<E>)
    requires(std::is_trivially_copy_constructible_v<T>) && std::is_trivially_copy_constructible_v<E>
  = default;

  constexpr expected(const expected& other) noexcept(
      std::is_nothrow_copy_constructible_v<T>&& std::is_nothrow_copy_constructible_v<E>)
    requires(expected_internal::is_non_trivially_copy_constructible<T>) &&
            expected_internal::is_non_trivially_copy_constructible<E>
      : has_value_(other.has_value_) {
    if (has_value()) {
      std::construct_at(std::addressof(data_.value), *other);
    } else {
      std::construct_at(std::addressof(data_.unexpected), other.error());
    }
  }

  constexpr expected(const expected& other) noexcept(
      std::is_nothrow_copy_constructible_v<T>&& std::is_nothrow_copy_constructible_v<E>)
    requires((!std::is_copy_constructible_v<T>) || !std::is_copy_constructible_v<E>)
  = delete;

  constexpr expected(expected&& other) noexcept(
      std::is_nothrow_move_constructible_v<T>&& std::is_nothrow_move_constructible_v<E>)
    requires std::is_trivially_move_constructible_v<T> && std::is_trivially_move_constructible_v<E>
  = default;

  constexpr expected(expected&& other) noexcept(
      std::is_nothrow_move_constructible_v<T>&& std::is_nothrow_move_constructible_v<E>)
    requires(expected_internal::is_non_trivially_move_constructible<T>) &&
            expected_internal::is_non_trivially_move_constructible<E>
      : has_value_(other.has_value_) {
    if (has_value()) {
      std::construct_at(std::addressof(data_.value), std::move(*other));
    } else {
      std::construct_at(std::addressof(data_.unexpected), std::move(other.error()));
    }
  }

  constexpr expected(expected&& other) noexcept(
      std::is_nothrow_move_constructible_v<T>&& std::is_nothrow_move_constructible_v<E>)
    requires((!std::is_move_constructible_v<T>) || !std::is_move_constructible_v<E>)
  = delete;

  template <class U, class G>
    requires(std::is_constructible_v<T, ref_t<const U>> && std::is_constructible_v<E, G&> &&
             convertible_value<U, G>)
  constexpr explicit(is_explicit<ref_t<const U>, const G&>) expected(
      const expected<U, G>& other) noexcept(std::is_nothrow_constructible_v<T, ref_t<const U>>&&
                                                std::is_nothrow_constructible_v<E, const G&>)
      : has_value_(other.has_value()) {
    if (has_value()) {
      std::construct_at(std::addressof(data_.value), std::forward<ref_t<const U>>(*other));
    } else {
      std::construct_at(std::addressof(data_.unexpected), std::forward<const G&>(other.error()));
    }
  }

  template <class U, class G>
    requires(std::is_constructible_v<T, U> && std::is_constructible_v<E, G> &&
             convertible_value<U, G>)
  constexpr explicit(is_explicit<U, G>) expected(expected<U, G>&& other) noexcept(
      std::is_nothrow_constructible_v<T, U>&& std::is_nothrow_constructible_v<E, G>)
      : has_value_(other.has_value()) {
    if (has_value()) {
      std::construct_at(std::addressof(data_.value), std::forward<U>(*other));
    } else {
      std::construct_at(std::addressof(data_.unexpected), std::forward<G>(other.error()));
    }
  }

  template <class U = T>
    requires(!expected_internal::any_type<U, expected, in_place_t> &&
             !expected_internal::unexpected_type<std::remove_cvref_t<U>> &&
             std::is_constructible_v<T, U>)
  constexpr explicit(!std::is_convertible_v<U, T>)
      expected(U&& v) noexcept(std::is_nothrow_constructible_v<T, E>)
      : data_(in_place_t{}, std::forward<U>(v)) {}

  template <class G = E>
    requires(std::is_constructible_v<E, const G&>)
  constexpr explicit(!std::is_constructible_v<const G&, E>)
      expected(const unexpected<G>& e) noexcept(std::is_nothrow_constructible_v<E, const G&>)
      : has_value_(false), data_(unexpect, std::forward<const G&>(e.error())) {}

  template <class G>
    requires(std::is_constructible_v<E, G>)
  constexpr explicit(!std::is_convertible_v<G, E>)
      expected(unexpected<G>&& e) noexcept(std::is_nothrow_constructible_v<E, G>)
      : has_value_(false), data_(unexpect, std::forward<G>(e.error())) {}

  template <typename... Args>
    requires(std::is_constructible_v<T, Args...>)
  constexpr explicit expected(in_place_t t,
                              Args&&... args) noexcept(std::is_nothrow_constructible_v<T, Args...>)
      : data_(t, std::forward<Args>(args)...) {}

  template <typename U, typename... Args>
    requires(std::is_constructible_v<T, std::initializer_list<U>&, Args...>)
  constexpr explicit expected(in_place_t t, std::initializer_list<U> il, Args&&... args) noexcept(
      std::is_nothrow_constructible_v<T, std::initializer_list<U>&, Args...>)
      : data_(t, il, std::forward<Args>(args)...) {}

  template <typename... Args>
    requires(std::is_constructible_v<E, Args...>)
  constexpr explicit expected(unexpect_t t,
                              Args&&... args) noexcept(std::is_nothrow_constructible_v<E, Args...>)
      : has_value_(false), data_(t, std::forward<Args>(args)...) {}

  template <typename U, typename... Args>
    requires(std::is_constructible_v<E, std::initializer_list<U>&, Args...>)
  constexpr explicit expected(unexpect_t t, std::initializer_list<U> il, Args&&... args) noexcept(
      std::is_nothrow_constructible_v<E, std::initializer_list<U>&, Args...>)
      : has_value_(false), data_(t, il, std::forward<Args>(args)...) {}

  // Trivial destructor when void or both trivially detructible.
  constexpr ~expected()
    requires(std::is_trivially_destructible_v<T> && std::is_trivially_destructible_v<E>)
  = default;

  // Non trivial destructor.
  constexpr ~expected()
    requires(!std::is_trivially_destructible_v<T> || !std::is_trivially_destructible_v<E>)
  {
    if (has_value()) {
      std::destroy_at(std::addressof(data_.value));
    } else {
      std::destroy_at(std::addressof(data_.unexpected));
    }
  }

  constexpr expected& operator=(const expected& rhs)
    requires(!expected_internal::is_copy_assignable<T, E>)
  = delete;

  constexpr expected& operator=(const expected& rhs) noexcept(
      std::is_nothrow_copy_constructible_v<T>&& std::is_nothrow_copy_assignable_v<T>&&
          std::is_nothrow_copy_constructible_v<E>&& std::is_nothrow_copy_assignable_v<T>)
    requires(expected_internal::is_copy_assignable<T, E>)
  {
    if (rhs) {
      set_value(*rhs);
    } else {
      set_unexpected(rhs.error());
    }
    return *this;
  }

  constexpr expected& operator=(expected&& rhs) noexcept
    requires(!expected_internal::is_move_assignable<T, E>)
  = delete;

  constexpr expected& operator=(expected&& rhs) noexcept(
      std::is_nothrow_move_constructible_v<T>&& std::is_nothrow_move_assignable_v<T>&&
          std::is_nothrow_move_constructible_v<T>&& std::is_nothrow_move_assignable_v<E>)
    requires(expected_internal::is_move_assignable<T, E>)
  {
    if (rhs) {
      set_value(std::move(*rhs));
    } else {
      set_unexpected(std::move(rhs.error()));
    }
    return *this;
  }

  template <typename U = T>
    requires(!expected_internal::expected_type<U> &&
             expected_internal::expected_assignable_from<T, E, U>)
  constexpr expected& operator=(U&& v) {
    set_value(std::forward<U>(v));
    return *this;
  }

  template <typename G>
  constexpr expected& operator=(const unexpected<G>& rhs)
    requires(expected_internal::expected_assignable_from_unexpected<T, E, const G&>)
  {
    set_unexpected(rhs.error());
    return *this;
  }

  template <typename G>
  constexpr expected& operator=(unexpected<G>&& rhs)
    requires(expected_internal::expected_assignable_from_unexpected<T, E, G>)
  {
    set_unexpected(std::move(rhs).error());
    return *this;
  }

  constexpr E& error() & noexcept {
    check_has_error();
    return data_.unexpected;
  }

  constexpr const E& error() const& noexcept {
    check_has_error();
    return data_.unexpected;
  }

  constexpr E&& error() && noexcept {
    check_has_error();
    return data_.unexpected;
  }

  constexpr const E&& error() const&& noexcept {
    check_has_error();
    return data_.unexpected;
  }

  constexpr T& value() & noexcept {
    check_has_value();
    return data_.value;
  }

  constexpr const T& value() const& noexcept {
    check_has_value();
    return data_.value;
  }

  constexpr T&& value() && noexcept {
    check_has_value();
    return std::move(data_.value);
  }

  constexpr const T&& value() const&& noexcept {
    check_has_value();
    return std::move(data_.value);
  }

  // This set of members have undefined behavior when |has_value| false.
  [[nodiscard]] constexpr T* operator->() noexcept {
    check_has_value();
    return std::addressof(data_.value);
  }

  [[nodiscard]] constexpr const T* operator->() const noexcept {
    check_has_value();
    return std::addressof(data_.value);
  }

  [[nodiscard]] constexpr T& operator*() & noexcept {
    check_has_value();
    return data_.value;
  }

  [[nodiscard]] constexpr const T& operator*() const& noexcept {
    check_has_value();
    return data_.value;
  }

  [[nodiscard]] constexpr T&& operator*() && noexcept {
    check_has_value();
    return std::move(data_.value);
  }

  [[nodiscard]] constexpr const T&& operator*() const&& noexcept {
    check_has_value();
    return std::move(data_.value);
  }

  // value_or
  template <class U>
    requires(std::is_copy_constructible_v<T> && std::is_convertible_v<U &&, T>)
  constexpr T value_or(U&& default_value) const& {
    return bool(*this) ? **this : static_cast<T>(std::forward<U>(default_value));
  }

  template <class U>
    requires(std::is_move_constructible_v<T> && std::is_convertible_v<U &&, T>)
  constexpr T value_or(U&& default_value) && {
    return has_value() ? **this : static_cast<T>(std::forward<U>(default_value));
  }

  constexpr bool has_value() const { return has_value_; }
  constexpr explicit operator bool() const { return has_value_; }

  // emplace
  template <typename... Args>
    requires(std::is_nothrow_constructible_v<T, Args...>)
  constexpr T& emplace(Args&&... args) noexcept {
    if (has_value()) {
      std::destroy_at(std::addressof(data_.value));
    } else {
      std::destroy_at(std::addressof(data_.unexpected));
      has_value_ = true;
    }
    std::construct_at(std::addressof(data_.value), std::forward<Args>(args)...);
    return **this;
  }

  template <typename U, typename... Args>
    requires(!std::is_void_v<T> &&
             std::is_nothrow_constructible_v<T, std::initializer_list<U>&, Args...>)
  constexpr T& emplace(std::initializer_list<U> il, Args&&... args) noexcept {
    if (has_value()) {
      std::destroy_at(std::addressof(data_.value));
    } else {
      std::destroy_at(std::addressof(data_.unexpected));
      has_value_ = true;
    }
    std::construct_at(std::addressof(data_.value), il, std::forward<Args>(args)...);
    return **this;
  }

  // swap
  constexpr void swap(expected& other) noexcept(
      std::is_nothrow_move_constructible_v<T>&& std::is_nothrow_swappable_v<T>&&
          std::is_nothrow_move_constructible_v<E>&& std::is_nothrow_swappable_v<E>)
    requires expected_internal::is_swappable<T, E>
  {
    if (has_value() && other.has_value()) {
      using std::swap;
      swap(data_.value, other.data_.value);
      return;
    }

    if (!has_value() && !other.has_value_) {
      using std::swap;
      swap(error(), other.error());
      return;
    }

    if (!this->has_value()) {
      other.swap(*this);
      return;
    }

    if constexpr (std::is_nothrow_move_constructible_v<E>) {
      expected_internal::value_guard<E> guard(other.data_.unexpected);
      std::construct_at(std::addressof(other.data_.value), std::move(data_.value));
      std::destroy_at(std::addressof(data_.value));
      std::construct_at(std::addressof(data_.unexpected), std::move(guard.release()));
      // Just flip the has value flags after swapping.
    } else {
      expected_internal::value_guard<T> guard(data_.value);
      std::construct_at(std::addressof(data_.unexpected), std::move(other.data_.unexpected));
      std::destroy_at(std::addressof(other.data_.unexpected));
      std::construct_at(std::addressof(other.data_.value), std::move(guard.release()));
    }
    has_value_ = false;
    other.has_value_ = true;
  }

  // Equality

  template <typename T2, typename E2>
    requires(!std::is_void_v<T2>)
  friend constexpr bool operator==(const expected<T, E>& lhs, const expected<T2, E2>& rhs) noexcept(
      noexcept(bool(*lhs == *rhs)) && noexcept(bool(rhs.error() == lhs.error()))) {
    if (lhs.has_value()) {
      return rhs.has_value() && *lhs == *rhs;
    }

    return !rhs.has_value() && lhs.error() == rhs.error();
  }

  template <typename T2>
  friend constexpr bool operator==(const expected& x,
                                   const T2& val) noexcept(noexcept(bool(*x == val))) {
    return x.has_value() && static_cast<bool>(*x == val);
  }

  template <typename E2>
  friend constexpr bool operator==(const expected& x, const unexpected<E2>& e) noexcept(
      noexcept(bool(x.error() == e.error()))) {
    return !x.has_value() && static_cast<bool>(x.error() == e.error());
  }

  friend constexpr void swap(expected<T, E>& x, expected<T, E>& y) noexcept(noexcept(x.swap(y))) {
    x.swap(y);
  }

 private:
  constexpr void check_has_value() const {
    if (!has_value()) {
      __builtin_abort();
    }
  }

  constexpr void check_has_error() const {
    if (has_value()) {
      __builtin_abort();
    }
  }

  template <typename U>
  constexpr void set_value(U&& v) {
    if (has_value()) {
      data_.value = std::forward<U>(v);
    } else {
      expected_internal::replace_value(data_.value, data_.unexpected, std::forward<U>(v));
      has_value_ = true;
    }
  }

  template <typename G>
  constexpr void set_unexpected(G&& v) {
    if (has_value()) {
      expected_internal::replace_value(data_.unexpected, data_.value, std::forward<G>(v));
      has_value_ = false;
    } else {
      data_.unexpected = std::forward<G>(v);
    }
  }

  bool has_value_ = true;

  expected_internal::storage<T, E> data_;
};

template <typename T, typename E>
  requires std::is_void_v<T> && expected_internal::unexpected_supported_type<E>
class expected<T, E> {
  static_assert(std::is_void_v<T>);
  // This checks that T and unexpected<U> are not constructible
  // from expected<U, G>.
  template <typename U, typename G>
  static constexpr bool convertible_value =
      !expected_internal::convertible_to_expected<T, U, G> &&
      !expected_internal::constructible_from_expected<T, U, G> &&
      !expected_internal::unexpected_constructible_from_expected<E, U, G>;

 public:
  using value_type = T;
  using error_type = E;
  using unexpected_type = unexpected<E>;

  template <typename U>
  using rebind = expected<U, error_type>;

  constexpr expected() noexcept
    requires(std::is_void_v<T>)
  = default;

  constexpr expected(const expected&) noexcept(std::is_nothrow_copy_constructible_v<E>)
    requires(std::is_trivially_copy_constructible_v<E>)
  = default;
  constexpr expected(const expected& other) noexcept(std::is_nothrow_copy_constructible_v<E>)
    requires(expected_internal::is_non_trivially_copy_constructible<E>)
      : has_value_(other.has_value()) {
    if (!has_value()) {
      std::construct_at(std::addressof(data_.unexpecte), other.error());
    }
  }
  constexpr expected(const expected& other)
    requires(!std::is_copy_constructible_v<E>)
  = delete;

  constexpr expected(expected&&) noexcept(std::is_nothrow_move_constructible_v<E>)
    requires(std::is_trivially_move_constructible_v<E>)
  = default;
  constexpr expected(expected&& other) noexcept(std::is_nothrow_move_constructible_v<E>)
    requires(expected_internal::is_non_trivially_move_constructible<E>)
      : has_value_(other.has_value()) {
    if (!has_value()) {
      std::construct_at(std::addressof(data_.unexpecte), std::move(other).error());
    }
  }
  constexpr expected(expected&& other)
    requires(!std::is_move_constructible_v<E>)
  = delete;

  template <typename U, typename G>
    requires(std::is_void_v<U> && std::is_constructible_v<E, const G&> && convertible_value<U, G>)
  constexpr explicit(!std::is_convertible_v<const G&, E>)
      expected(const expected<U, G>& other) noexcept(std::is_nothrow_constructible_v<E, const G&>)
      : has_value_(other.has_value()) {
    if (!has_value()) {
      std::construct_at(std::addressof(data_.unexpecte), other.error());
    }
  }

  template <typename U, typename G>
    requires(std::is_void_v<U> && std::is_constructible_v<E, G> && convertible_value<U, G>)
  constexpr explicit(!std::is_convertible_v<G, E>)
      expected(expected<U, G>&& other) noexcept(std::is_nothrow_constructible_v<E, G>)
      : has_value_(other.has_value()) {
    if (!has_value()) {
      std::construct_at(std::addressof(data_.unexpecte), std::move(other).error());
    }
  }

  template <typename G = E>
    requires std::is_constructible_v<E, const G&>
  constexpr explicit(!std::is_convertible_v<const G&, E>)
      expected(const unexpected<G>& u) noexcept(std::is_nothrow_constructible_v<E, const G&>)
      : has_value_(false), data_(unexpect, u.error()) {}

  template <typename G = E>
    requires std::is_constructible_v<E, G>
  constexpr explicit(!std::is_convertible_v<G, E>)
      expected(unexpected<G>&& u) noexcept(std::is_nothrow_constructible_v<E, G>)
      : has_value_(false), data_(unexpect, std::move(u).error()) {}

  template <typename... _Args>
  constexpr explicit expected(in_place_t) noexcept : expected() {}

  template <typename... Args>
    requires std::is_constructible_v<E, Args...>
  constexpr explicit expected(unexpect_t t,
                              Args&&... args) noexcept(std::is_nothrow_constructible_v<E, Args...>)
      : has_value_(false), data_(t, std::forward<Args>(args)...) {}

  template <typename U, typename... Args>
    requires std::is_constructible_v<E, std::initializer_list<U>&, Args...>
  constexpr explicit expected(unexpect_t t, std::initializer_list<U> il, Args&&... args) noexcept(
      std::is_nothrow_constructible_v<E, std::initializer_list<U>&, Args...>)
      : has_value_(false), data_(t, il, std::forward<Args>(args)...) {}

  constexpr ~expected()
    requires(std::is_trivially_destructible_v<E>)
  = default;

  constexpr ~expected()
    requires(!std::is_trivially_destructible_v<E>)
  {
    if (!has_value()) {
      std::destroy_at(std::addressof(data_.unexpected));
    }
  }

  constexpr expected& operator=(const expected&) noexcept
    requires(std::is_trivially_copy_assignable_v<E> && std::is_trivially_copy_constructible_v<T>)
  = default;

  constexpr expected& operator=(const expected& rhs) noexcept(
      std::is_nothrow_copy_constructible_v<E>&& std::is_nothrow_copy_assignable_v<T>)
    requires(expected_internal::is_non_trivially_copy_constructible<E> &&
             expected_internal::is_copy_assignable<T, E>)
  {
    if (!rhs.has_value()) {
      set_unexpected(rhs.error());
    } else {
      emplace();
    }
    return *this;
  }

  constexpr expected& operator=(const expected&)
    requires(!std::is_copy_assignable_v<E>)
  = delete;

  constexpr expected& operator=(expected&&) noexcept
    requires(std::is_trivially_move_assignable_v<E> && std::is_trivially_move_constructible_v<T>)
  = default;

  constexpr expected& operator=(expected&& rhs) noexcept(
      std::is_nothrow_move_constructible_v<E>&& std::is_nothrow_move_assignable_v<T>)
    requires(expected_internal::is_non_trivially_move_constructible<E> &&
             expected_internal::is_move_assignable<T, E>)
  {
    if (!rhs.has_value()) {
      set_unexpected(std::move(rhs).error());
    } else {
      emplace();
    }
    return *this;
  }

  constexpr expected& operator=(expected&&)
    requires(!std::is_move_assignable_v<E>)
  = delete;

  template <typename G>
    requires std::is_constructible_v<E, const G&> && std::is_assignable_v<E, const G&>
  constexpr expected& operator=(const unexpected<G>& e) {
    set_unexpected(e.error());
    return *this;
  }

  template <typename G>
    requires std::is_constructible_v<E, G> && std::is_assignable_v<E, G>
  constexpr expected& operator=(unexpected<G>&& e) {
    set_unexpected(std::move(e).error());
    return *this;
  }

  constexpr void emplace() noexcept(std::is_nothrow_destructible_v<E>) {
    if (!has_value()) {
      std::destroy_at(std::addressof(data_.unexpected));
      has_value_ = true;
    }
  }

  constexpr void swap(expected& other) noexcept(
      std::is_nothrow_swappable_v<E&>&& std::is_nothrow_move_constructible_v<E>)
    requires std::is_swappable_v<E> && std::is_move_constructible_v<E>
  {
    if (!has_value()) {
      if (other.has_value()) {
        other.swap(*this);
        return;
      }
      using std::swap;
      swap(data_.unexpected, other.error());
      return;
    }

    if (!other.has_value()) {
      set_unexpected(std::move(other).error());
      other.emplace();
    }
  }

  [[nodiscard]] constexpr explicit operator bool() const noexcept { return has_value_; }
  [[nodiscard]] constexpr bool has_value() const { return has_value_; }

  constexpr void operator*() noexcept { check_has_value(); }
  constexpr void value() const& { check_has_value(); }
  constexpr void value() && { check_has_value(); }

  constexpr E& error() & noexcept {
    check_has_error();
    return data_.unexpected;
  }

  constexpr const E& error() const& noexcept {
    check_has_error();
    return data_.unexpected;
  }

  constexpr E&& error() && noexcept {
    check_has_error();
    return std::move(data_.unexpected);
  }

  constexpr const E&& error() const&& noexcept {
    check_has_error();
    return std::move(data_.unexpected);
  }

  friend constexpr void swap(expected& lhs, expected& rhs) noexcept(noexcept(rhs.swap(lhs)))
    requires requires { rhs.swap(lhs); }
  {
    lhs.swap(rhs);
  }

  template <typename T2, typename E2>
    requires std::is_void_v<T2>
  friend constexpr bool operator==(const expected& lhs, const expected<T2, E2>& rhs) noexcept(
      noexcept(bool(lhs.error() == rhs.error()))) {
    if (lhs.has_value()) {
      return rhs.has_value();
    }
    return !rhs.has_value() && bool(lhs.error() == rhs.error());
  }

  template <typename E2>
  friend constexpr bool operator==(const expected& lhs, const unexpected<E2>& rhs) noexcept(
      noexcept(bool(lhs.error() == rhs.error()))) {
    return !lhs.has_value() && bool(lhs.error() == rhs.error());
  }

 private:
  template <typename T2, typename E2>
  friend class expected;

  constexpr void check_has_error() const {
    if (has_value()) {
      __builtin_abort();
    }
  }

  constexpr void check_has_value() const {
    if (!has_value()) {
      __builtin_abort();
    }
  }

  template <typename G>
  constexpr void set_unexpected(G&& v) {
    if (has_value()) {
      std::construct_at(std::addressof(data_.unexpected), std::forward<G>(v));
      has_value_ = false;
    } else {
      data_.unexpected = std::forward<G>(v);
    }
  }

  bool has_value_ = true;

  struct empty_t {};

  expected_internal::storage<empty_t, E> data_;
};

}  // namespace base
