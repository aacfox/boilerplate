module;
#define cauto const auto
#include <execution>
#include <gsl/gsl>
export module boilerplate;
export import std;

export {
  using namespace std;
  namespace exe = execution;
  namespace file = filesystem;
  namespace c = chrono;
  namespace e = execution;
  namespace f = filesystem;
  namespace r = ranges;
  namespace v = views;
  using gsl::at, gsl::final_action, gsl::finally, gsl::index, gsl::narrow, gsl::narrow_cast,
       ranges::forward_range, ranges::range, ranges::borrowed_range,
      ranges::random_access_range, ranges::sized_range, ranges::common_range,
      ranges::sized_range, ranges::view;

  namespace boil {
  inline namespace utilities {
  template <class N>
  concept arithmetic = floating_point<N> or integral<N>;
  template <class Tuple, size_t size = 0>
  concept tuple_like = tuple_size_v<remove_cvref_t<Tuple>> >= size;
  template <class Pair>
  concept pair_like = tuple_size_v<remove_cvref_t<Pair>> == 2;
  template <arithmetic T> constexpr auto max_v = numeric_limits<T>::max();
  template <arithmetic T> constexpr auto min_v = numeric_limits<T>::min();
  constexpr auto now = &high_resolution_clock::now;
  using cache_line = bitset<hardware_constructive_interference_size>;
  enum class order : signed char {
    equal = 0,
    equivalent = equal,
    less = -1,
    greater = 1,
    unordered = -128,
  };
  [[nodiscard]] constexpr auto today() noexcept {
    return year_month_day{floor<days>(system_clock::now())};
  }
  [[nodiscard]] constexpr auto o_clock() noexcept {
    return hh_mm_ss{floor<seconds>(system_clock::now()) -
                    floor<days>(system_clock::now())};
  }
  [[nodiscard]] constexpr auto subrange(pair_like auto pair) noexcept {
    static_assert(sentinel_for<std::tuple_element_t<1, decltype(pair)>,
                               std::tuple_element_t<0, decltype(pair)>>,
                  "Denotes invalid subrange!");
    return r::subrange{get<0>(pair), get<1>(pair)};
  }
  auto create_file(convertible_to<path> auto &&filename) {
    ofstream{filename};
    return true;
  }
  auto create_file(convertible_to<path> auto &&filename,
                   error_code &error) noexcept try {
    create_file(forward(filename));
    error.clear();
    return true;
  } catch (...) {
    error = make_error_code(io_errc::stream);
    return false;
  }

  class Exception : public exception {
    // clang-format off
  public:
  explicit constexpr Exception(string_view what = "Unknown exception.",
        source_location where = source_location::current()
        #ifdef __cpp_lib_stacktrace
        , stacktrace when = stacktrace::current(1)
        #endif
        ): _what{std::move(what)}, _where{std::move(where)}
        #ifdef __cpp_lib_stacktrace
          ,_when{std::move(when)}
        #endif
  {}
  [[nodiscard]] auto what() const noexcept -> const char * override { return _what.data(); }
  [[nodiscard]] virtual auto where() const noexcept -> const source_location& { return _where; }
  #ifdef __cpp_lib_stacktrace
  [[nodiscard]] virtual when() const noexcept -> const stacktrace& { return _when; }
  #endif

  private:
  string_view _what;
  const source_location _where;
  #ifdef __cpp_lib_stacktrace
  const stacktrace _when;
  #endif
    // clang-format on
  };
  } // namespace utilities

  inline namespace classes {
  class bitvector : public vector<bool> {
  public:
    using vector<bool>::vector;
    template <size_t N = 0> constexpr explicit bitvector(bitset<N> &rhs);
    template <size_t N = 0>
    [[nodiscard]] constexpr operator bitset<N>(this const bitvector &);
    [[nodiscard]] constexpr bool operator[](this const bitvector &, index bit);
    [[nodiscard]] reference operator[](this bitvector &, index bit);
    [[nodiscard]] constexpr bool test(this const bitvector &, index bit);
    [[nodiscard]] constexpr bool operator==(this const bitvector &,
                                            const bitvector &rhs);
    [[nodiscard]] constexpr bool all(this const bitvector &);
    [[nodiscard]] constexpr bool any(this const bitvector &);
    [[nodiscard]] constexpr bool none(this const bitvector &);
    [[nodiscard]] constexpr size_t count(this const bitvector &);
    bitvector &operator&=(this bitvector &, const bitvector &other);
    bitvector &operator|=(this bitvector &, const bitvector &other);
    bitvector &operator^=(this bitvector &, const bitvector &other);
    [[nodiscard]] constexpr bitvector operator~(this const bitvector);
    bitvector &operator<<=(this bitvector &, size_t shift);
    [[nodiscard]] constexpr bitvector operator<<(this const bitvector,
                                                 size_t shift);
    bitvector &operator>>=(this bitvector &, size_t shift);
    [[nodiscard]] constexpr bitvector operator>>(this const bitvector &,
                                                 size_t shift);
    bitvector &set(this bitvector &);
    bitvector &set(this bitvector &, index bit, bool value = true);
    bitvector &reset(this bitvector &);
    bitvector &reset(this bitvector &, index bit);
    bitvector &flip(this bitvector &);
    bitvector &flip(this bitvector &, index bit);

  private:
  };
  using dynamic_bitset = bitvector;
  } // namespace classes

  inline namespace functions {
  template <arithmetic T = size_t>
  [[nodiscard]] auto randomizer(T min = min_v<T>, T max = max_v<T>) {
    random_device seeder;
    default_random_engine generator{seeder()};
    if constexpr (is_floating_point_v<T>) {
      return bind(uniform_real_distribution{min, max}, std::move(generator));
    }
    if constexpr (is_integral_v<T>) {
      return bind(uniform_int_distribution{min, max}, std::move(generator));
    }
  }

  template <size_t n = 1024, class... Args, invocable<Args...> Fx>
  [[nodiscard]] auto benchmark(Fx &&fx, Args &&...args) {
    const auto epoch = boil::now();
    for (size_t i{}; i != n; ++i) {
      invoke(std::forward<Fx>(fx), std::forward<Args>(args)...);
    }
    return (boil::now() - epoch) / n;
  }

  template <class T, class Projection = std::identity>
  void radix_sort(std::span<const T> span, Projection projection = {},
                  bool LSD = true) {
    //   const size_t size{span.size()};
    //   constexpr size_t bits{sizeof(std::invoke(projection, span[0])) * 8};
    //   using bitset = std::bitset<bits>;
    //   constexpr unsigned char base{4};
    //   constexpr unsigned char radix{1 << base}; // = 16, given base == 4
    //   constexpr size_t length{bits / base};
    //   std::vector<T> spanCopy;
    //   spanCopy.reserve(size);
    //   std::vector<bitset> sets;
    //   sets.reserve(size);
    //   for (auto i : span) {
    //     spanCopy.push_back(i);
    //     sets.push_back(std::bit_cast<bitset>(std::invoke(projection, i)));
    //   }
    //   auto change_radix{[&](bitset &set, std::vector<size_t> &digits) {
    //     for (size_t digit{}; digit < length; ++digit) {
    //       std::bitset<8> mask{};
    //       set >>= base;
    //       for (size_t i{}; i < base; ++i)
    //         if (set.test(i))
    //           mask.set(i);
    //       digits.push_back(std::bit_cast<int>(mask));
    //     }
    //   }};
    //   std::vector<std::tuple<size_t, std::vector<size_t>>> to_sort(size);
    //   for (size_t i{}; auto &[index, digits] : to_sort) {
    //     index = i++;
    //     change_radix(sets.at(index), digits);
    //   }
    //   sets.clear();
    //   std::array<size_t, radix> buckets{};
    //   auto countingSort{[&](const size_t digit) mutable {
    //     buckets.fill(0);
    //     auto toSortCopy{to_sort};
    //     for (const auto &[index, digits] : toSortCopy)
    //       ++buckets.at(digits.at(digit));
    //     std::inclusive_scan(std::begin(buckets), std::end(buckets),
    //                         std::begin(buckets));
    //     for (auto &i : toSortCopy | reverse) {
    //       const size_t index = std::get<1>(i).at(digit);
    //       to_sort.at(--buckets.at(index)) = std::move(i);
    //     }
    //   }};
    //   for (size_t i{}; i != length; ++i) {

    //     countingSort(i);
    //   }
    //   for (size_t i{}; auto &[index, garbage] : to_sort)
    //     span[i++] = std::move(spanCopy.at(index));
  }
  } // namespace functions
  } // namespace boil
}

// bitvector imple
namespace boil {
template <size_t N = 0> constexpr bitvector::bitvector(bitset<N> &rhs) {
  this->reserve(rhs.size());
  for (size_t _{}; _ != rhs.size(); ++_) {
    this->push_back(rhs[_]);
  }
}

template <size_t N = 0>
constexpr bitvector::operator bitset<N>(this const bitvector &self) {
  Expects(N >= self.size());
  return bitset<N>{self | v::transform([](auto _) { return '0' + _; }) |
                   v::reverse | r::to<string>()};
}

constexpr bool bitvector::operator[](this const bitvector &self,
                                     const index bit) {
  return self[bit];
}

bitvector::reference bitvector::operator[](this bitvector &self,
                                           const index bit) {
  return self[bit];
}

constexpr bool bitvector::test(this const bitvector &self,
                               const index bit) {
  Expects(bit < self.size());
  return self[bit];
}

constexpr bool bitvector::operator==(this const bitvector &self,
                                     const bitvector &rhs) {
  return r::equal(self, rhs);
}

constexpr bool bitvector::all(this const bitvector &self) {
  return r::all_of(self, identity{});
}

constexpr bool bitvector::any(this const bitvector &self) {
  return r::any_of(self, identity{});
}

constexpr bool bitvector::none(this const bitvector &self) {
  return r::none_of(self, identity{});
}

constexpr size_t bitvector::count(this const bitvector &self) {
  return r::count(self, true);
}

bitvector &bitvector::operator&=(this bitvector &self, const bitvector &other) {
  r::transform(self, other, self.begin(),
               [](auto &&lhs, auto &&rhs) { return lhs && rhs; });
  return self;
}

bitvector &bitvector::operator|=(this bitvector &self, const bitvector &other) {
  r::transform(self, other, self.begin(),
               [](auto &&lhs, auto &&rhs) { return lhs || rhs; });
  return self;
}

bitvector &bitvector::operator^=(this bitvector &self, const bitvector &other) {
  r::transform(self, other, self.begin(),
               [](auto &&lhs, auto &&rhs) { return lhs != rhs; });
  return self;
}
bitvector &bitvector::operator<<=(this bitvector &self, const size_t shift) {
  shift_left(self.begin(), self.end(), shift);
  r::fill_n(self.rbegin(), shift, false);
  return self;
}

constexpr bitvector bitvector::operator<<(this const bitvector self,
                                          const size_t shift) {
  return const_cast<bitvector &>(self) <<= shift;
}
bitvector &bitvector::operator>>=(this bitvector &self, const size_t shift) {
  shift_right(self.begin(), self.end(), shift);
  r::fill_n(self.begin(), shift, false);
  return self;
}

constexpr bitvector bitvector::operator>>(this const bitvector &self,
                                          const size_t shift) {
  return const_cast<bitvector &>(self) >>= shift;
}
bitvector &bitvector::set(this bitvector &self) {
  r::fill(self, true);
  return self;
}

bitvector &bitvector::set(this bitvector &self, const index bit,
                          bool value) {
  self[bit] = value;
  return self;
}

bitvector &bitvector::reset(this bitvector &self) {
  r::fill(self, false);
  return self;
}

bitvector &bitvector::reset(this bitvector &self, const index bit) {
  self[bit] = false;
  return self;
}

bitvector &bitvector::flip(this bitvector &self) {
  for (index _{}; _ != self.size(); ++_) {
    self[_].flip();
  }
  return self;
}

bitvector &bitvector::flip(this bitvector &self, const index bit) {
  self[bit].flip();
  return self;
}

constexpr bitvector bitvector::operator~(this const bitvector self) {
  return const_cast<bitvector &>(self).flip();
}
} // namespace boil
