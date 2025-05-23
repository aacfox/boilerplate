module;
#include <execution>
#include <gsl/gsl>
export module boilerplate;
export import std;

// DONE(aacfox):

export {
  using namespace std;
  using namespace chrono;
  using namespace execution;
  using namespace filesystem;
  namespace exe = execution;
  namespace file = filesystem;
  namespace c = chrono;
  namespace e = execution;
  namespace f = filesystem;
  namespace r = ranges;
  namespace v = views;
  using gsl::at, gsl::final_action, gsl::finally, gsl::index, gsl::narrow,
      gsl::narrow_cast, gsl::not_null, ranges::forward_range, ranges::range,
      ranges::borrowed_range, ranges::random_access_range, ranges::sized_range,
      ranges::common_range, ranges::sized_range, ranges::view;
  // TODO(aacfox): how to export macro now?
  namespace boil {
  inline namespace sugars { // syntactic ones
  template <class T>
  concept character = requires { typename char_traits<T>; };

  template <class N>
  concept arithmetic = is_arithmetic_v<N>;

  template <class Tuple, size_t size = 0>
  concept tuple_like = tuple_size_v<remove_cvref_t<Tuple>> >= size;

  template <class Pair>
  concept pair_like = tuple_size_v<remove_cvref_t<Pair>> == 2;

  template <arithmetic T> constexpr auto max_v = numeric_limits<T>::max();
  template <arithmetic T> constexpr auto min_v = numeric_limits<T>::min();

  constexpr auto now = &high_resolution_clock::now;

  using cache_line = bitset<hardware_constructive_interference_size>;
  } // namespace sugars
  inline namespace utilities { // tiny ones
  enum class Order : signed char {
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

  [[nodiscard]] constexpr auto subrange(pair_like auto _) noexcept {
    Expects((sentinel_for<tuple_element_t<1, decltype(_)>,
                          tuple_element_t<0, decltype(_)>>));
    return r::subrange{get<0>(_), get<1>(_)};
  }

  void print_type(auto var) { // removes cv-qualifiers!
    // TODO(aacfox): maybe return somehow internally?..
    system(("c++filt -t "s + typeid(std::forward<decltype(var)>(var)).name())
               .data());
    print("\r");
  }

  auto create_file(convertible_to<path> auto &&filename) {
    ofstream{filename};
    return true;
  }
  auto create_file(convertible_to<path> auto &&filename,
                   error_code &error) noexcept try {
    create_file(std::forward<decltype(filename)>(filename));
    error.clear();
    return true;
  } catch (...) {
    error = make_error_code(io_errc::stream);
    return false;
  }

  template <character CharT = char>
  basic_stringstream<CharT> all_contents(ifstream &in) {
    return {istreambuf_iterator<CharT>{in}, istreambuf_iterator<CharT>{}};
  }

  class Exception : public exception { // clang-format off
    // TODO(aacfox): nested exceptions
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
  [[nodiscard]] constexpr const char* what() const noexcept override { return _what.data(); }
  [[nodiscard]] virtual constexpr const source_location& where() const noexcept { return _where; }
  #ifdef __cpp_lib_stacktrace
  [[nodiscard]] virtual const stacktrace& when() const noexcept { return _when; }
  #endif

  private:
  string_view _what;
  const source_location _where;
  #ifdef __cpp_lib_stacktrace
  const stacktrace _when;
  #endif // clang-format on
  };

  [[nodiscard]] constexpr string_view
  what(const exception_ptr &eptr = current_exception()) noexcept try {
    if (eptr) {
      rethrow_exception(eptr);
    } else {
      return "No exception.";
    }
  } catch (const exception &e) {
    return e.what();
  } catch (string_view e) {
    return e;
  } catch (const string &e) {
    return e;
  } catch (const char *e) {
    return e;
  } catch (...) {
    return "Unknown/No message.";
  }

  [[nodiscard]] constexpr source_location
  where(const exception_ptr &eptr = current_exception()) noexcept try {
    if (eptr) {
      rethrow_exception(eptr);
    } else {
      return {};
    }
  } catch (const Exception &e) {
    return e.where();
  } catch (...) {
    return {};
  }

#ifdef __cpp_lib_stacktrace
  [[nodiscard]] stacktrace
  when(const exception_ptr &eptr = current_exception()) noexcept {}
#endif
  } // namespace utilities
  inline namespace classes {
  class bitvector : public vector<bool> {
    // TODO(aacfox): will need integer_sequence or iota? if already constexpr...
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
  [[nodiscard]] auto randomizer(const T lower_bound, const T upper_bound) {
    random_device seeder;
    default_random_engine generator{seeder()};
    if constexpr (is_floating_point_v<T>) {
      return bind(uniform_real_distribution{lower_bound, upper_bound},
                  std::move(generator));
    }
    if constexpr (is_integral_v<T>) {
      return bind(uniform_int_distribution{lower_bound, upper_bound},
                  std::move(generator));
    }
  }
  template <arithmetic T = size_t>
  [[nodiscard]] auto randomizer(const T from_zero_to) {
    return randomizer<T>(0, from_zero_to);
  }
  template <arithmetic T = size_t> [[nodiscard]] auto randomizer() {
    return randomizer<T>(min_v<T>, max_v<T>);
  }
  [[nodiscard]] bool flip_coin() {
    thread_local auto coin{randomizer<size_t>(0, 1)};
    return coin();
  }

  template <size_t n = 1024, class... Args, invocable<Args...> Fx>
  [[nodiscard]] auto benchmark(Fx &&fx, Args &&...args) {
    const auto epoch = now();
    for (size_t i{}; i != n; ++i) {
      invoke(std::forward<Fx>(fx), std::forward<Args>(args)...);
    }
    return (now() - epoch) / n;
  }
  template <size_t precision = 1024, class... Args, invocable<Args...> Fx>
  [[nodiscard]] auto benchmark_to_death(Fx &&fx, Args &&...args) {
    auto last_average{1ns}; // so that it doesn't stop on the first iteration
    for (auto [total, times, buffer, same_in_row] = // clang-format off
         tuple{  0ns,   0UZ,    0ns,         0UZ};
         same_in_row != precision;
         total += benchmark(std::forward<Fx>(fx), std::forward<Args>(args)...),
         last_average = exchange(buffer, total / ++times)) // clang-format on
      if (last_average == buffer)
        ++same_in_row;
      else
        same_in_row = 0;
    return last_average;
  };

  template <class T, class Projection = identity>
  void radix_sort(span<const T> span, Projection projection = {},
                  bool LSD = true) {
    //   const size_t size{span.size()};
    //   constexpr size_t bits{sizeof(invoke(projection, span[0])) * 8};
    //   using bitset = bitset<bits>;
    //   constexpr unsigned char base{4};
    //   constexpr unsigned char radix{1 << base}; // = 16, given base == 4
    //   constexpr size_t length{bits / base};
    //   vector<T> spanCopy;
    //   spanCopy.reserve(size);
    //   vector<bitset> sets;
    //   sets.reserve(size);
    //   for (auto i : span) {
    //     spanCopy.push_back(i);
    //     sets.push_back(bit_cast<bitset>(invoke(projection, i)));
    //   }
    //   auto change_radix{[&](bitset &set, vector<size_t> &digits) {
    //     for (size_t digit{}; digit < length; ++digit) {
    //       bitset<8> mask{};
    //       set >>= base;
    //       for (size_t i{}; i < base; ++i)
    //         if (set.test(i))
    //           mask.set(i);
    //       digits.push_back(bit_cast<int>(mask));
    //     }
    //   }};
    //   vector<tuple<size_t, vector<size_t>>> to_sort(size);
    //   for (size_t i{}; auto &[index, digits] : to_sort) {
    //     index = i++;
    //     change_radix(sets.at(index), digits);
    //   }
    //   sets.clear();
    //   array<size_t, radix> buckets{};
    //   auto countingSort{[&](const size_t digit) mutable {
    //     buckets.fill(0);
    //     auto toSortCopy{to_sort};
    //     for (const auto &[index, digits] : toSortCopy)
    //       ++buckets.at(digits.at(digit));
    //     inclusive_scan(begin(buckets), end(buckets),
    //                         begin(buckets));
    //     for (auto &i : toSortCopy | reverse) {
    //       const size_t index = get<1>(i).at(digit);
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

namespace boil { // bitvector imple
template <size_t N = 0> constexpr bitvector::bitvector(bitset<N> &rhs) {
  this->reserve(rhs.size());
  for (size_t _{}; _ != rhs.size(); ++_) {
    this->push_back(rhs[_]);
  }
}

template <size_t N = 0>
constexpr bitvector::operator bitset<N>(this const bitvector &self) {
  Expects((N >= self.size()));
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

constexpr bool bitvector::test(this const bitvector &self, const index bit) {
  Expects((bit < self.size()));
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

bitvector &bitvector::set(this bitvector &self, const index bit, bool value) {
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

module :private;
// please, don't ask anything about the following...
// chrono::nanoseconds,
// chrono::microseconds,
// chrono::milliseconds,
// chrono::seconds,
// chrono::minutes,
// chrono::hours,
// chrono::days,
// chrono::months,
// chrono::years,
// chrono::hh_mm_ss,
// chrono::year_month_day,