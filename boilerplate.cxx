export module boilerplate;
export import std;

// DONE(aacfox):

export {
  using namespace std;
  using namespace chrono;
  // TODO(aacfox): experimental 'import std;'
  // seems to be incompetible with ' "-fexperimental-library"
  // using namespace execution;
  using namespace filesystem;
  // namespace exe = execution;
  namespace file = filesystem;
  namespace c = chrono;
  // namespace e = execution;
  namespace f = filesystem;
  namespace r = ranges;
  namespace v = views;
  using gsl::at, gsl::final_action, gsl::finally, gsl::index, gsl::narrow,
      gsl::narrow_cast, gsl::not_null, ranges::forward_range, ranges::range,
      ranges::borrowed_range, ranges::random_access_range, ranges::sized_range,
      ranges::common_range, ranges::sized_range, ranges::view;

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

  template <arithmetic T> constexpr auto //
      max_v = numeric_limits<T>::max();
  template <arithmetic T> constexpr auto //
      min_v = numeric_limits<T>::min();
  const auto //
      now = &high_resolution_clock::now;

  using cache_line = bitset<hardware_constructive_interference_size>;

  [[nodiscard]] auto //
  today() noexcept {
    return year_month_day{floor<days>(system_clock::now())};
  }

  [[nodiscard]] auto //
  o_clock() noexcept {
    return hh_mm_ss{floor<seconds>(system_clock::now()) -
                    floor<days>(system_clock::now())};
  }

  [[nodiscard]] constexpr auto //
  subrange(pair_like auto &&iter_pair) noexcept {
    Expects((sentinel_for<tuple_element_t<1, decltype(iter_pair)>,
                          tuple_element_t<0, decltype(iter_pair)>>));
    return r::subrange{get<0>(self_forward(iter_pair)),
                       get<1>(self_forward(iter_pair))};
  }
  } // namespace sugars
  inline namespace utilities { // tiny ones
  enum class Order : signed char {
    equal = 0,
    equivalent = equal,
    less = -1,
    greater = 1,
    unordered = -128,
  };

  auto //
  print_type(auto &&var) {
    // TODO(aacfox): maybe return somehow internally?..
    auto _ = system(("c++filt -t "s + typeid(self_forward(var)).name()).data());
    print("\r");
    return _;
  }

  auto                                              //
  create_file(convertible_to<path> auto &&filename) //
      -> directory_entry {
    ofstream{filename};
    return {filename};
  }
  auto                                                                 //
  create_file(convertible_to<path> auto &&filename, error_code &error) //
      noexcept -> directory_entry try {
    create_file(self_forward(filename));
    error.clear();
    return {filename};
  } catch (...) {
    error = make_error_code(io_errc::stream);
    return {filename};
  }

  template <character Char = char> [[nodiscard]] auto //
  all_contents(ifstream &input)                       //
      -> basic_stringstream<Char> {
    return {istreambuf_iterator<Char>{input}, istreambuf_iterator<Char>{}};
  }

  class Exception : public exception { // clang-format off
    // TODO(aacfox): nested exceptions
  public:
    explicit Exception( // NOLINT: false positive
        string_view what = "Unknown exception.", // NOLINT: it won't be, when stacktrace is adopted
#ifdef __cpp_lib_stacktrace
        stacktrace when = stacktrace::current(1),
#endif
        source_location where = source_location::current())
        : _what{std::move(what)},
#ifdef __cpp_lib_stacktrace
          _when{std::move(when)},
#endif
          _where{std::move(where)} {}
    [[nodiscard]] const char * //
    what() const noexcept override { return _what.data(); }
#ifdef __cpp_lib_stacktrace
    [[nodiscard]] virtual 
    auto when() const noexcept -> const stacktrace & { return _when; }
#endif
    [[nodiscard]] virtual auto //
    where() const noexcept -> const source_location & { return _where; }
  private:
    string_view _what;
#ifdef __cpp_lib_stacktrace
    stacktrace _when;
#endif
    source_location _where;
  }; // clang-format on

  template <class = void> [[nodiscard]] constexpr auto  //
  what(const exception_ptr &eptr = current_exception()) //
      noexcept -> string_view try {
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

  template <class = void> [[nodiscard]] constexpr auto   //
  where(const exception_ptr &eptr = current_exception()) //
      noexcept -> source_location try {
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
  template <class = void> [[nodiscard]] auto            //
  when(const exception_ptr &eptr = current_exception()) //
      noexcept -> stacktrace {}
#endif
  } // namespace utilities
  inline namespace classes {
  template <class = void>
  class bitvector : public vector<bool> {
  public:
    using vector<bool>::vector;
    template <size_t N = 0> constexpr explicit //
        bitvector(const bitset<N> &rhs);
    template <size_t N = 0> [[nodiscard]] constexpr //
    operator bitset<N>(this const bitvector &);
    [[nodiscard]] constexpr bool //
    test(this const bitvector &, index bit);
    [[nodiscard]] constexpr bool //
    all(this const bitvector &);
    [[nodiscard]] constexpr bool //
    any(this const bitvector &);
    [[nodiscard]] constexpr bool //
    none(this const bitvector &);
    [[nodiscard]] constexpr size_t //
    count(this const bitvector &);
    [[nodiscard]] constexpr bitvector //
    operator<<(this bitvector, size_t shift);
    [[nodiscard]] constexpr bitvector //
    operator>>(this bitvector, size_t shift);
    [[nodiscard]] constexpr bitvector //
    operator~(this bitvector);
    bitvector & //
    operator&=(this bitvector &, const bitvector &other);
    bitvector & //
    operator|=(this bitvector &, const bitvector &other);
    bitvector & //
    operator^=(this bitvector &, const bitvector &other);
    bitvector & //
    operator<<=(this bitvector &, size_t shift);
    bitvector & //
    operator>>=(this bitvector &, size_t shift);
    bitvector & //
    set(this bitvector &);
    bitvector & //
    set(this bitvector &, index bit, bool value = true);
    bitvector & //
    reset(this bitvector &);
    bitvector & //
    reset(this bitvector &, index bit);
    bitvector & //
    flip(this bitvector &, index bit);
    using vector<bool>::flip;

  private:
  };
  using dynamic_bitset = bitvector<void>;

  template <class T>
  class vantage_ptr : public not_null<T *> {
  public:
    using pointer = T *;
    using element_type = T;
    vantage_ptr(indirectly_readable auto &&ptr)
        : not_null<pointer>{to_address(self_forward(ptr))} {}
    vantage_ptr(auto &&arg) : not_null<pointer>{addressof(arg)} {}
    operator pointer() const = delete; // YOU DON'T OWN THE POINTEE
    pointer get() const = delete;      // YOU DON'T OWN THE POINTEE
  };
  template <indirectly_readable T> vantage_ptr(T &&t)
      -> vantage_ptr<remove_reference_t<decltype(*t)>>;
  template <class T> vantage_ptr(T &&) -> vantage_ptr<remove_reference_t<T>>;

  [[nodiscard]] auto //
  make_vantage(auto &&arg) {
    return vantage_ptr{self_forward(arg)};
  }
  } // namespace classes
  inline namespace functions {
  template <arithmetic T = size_t> [[nodiscard]] auto //
  randomizer(const T lower_bound, const T upper_bound) {
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
  template <arithmetic T = size_t> [[nodiscard]] auto //
  randomizer(const T from_zero_to) {
    return randomizer<T>(0, from_zero_to);
  }
  template <arithmetic T = size_t> [[nodiscard]] auto //
  randomizer() {
    return randomizer<T>(min_v<T>, max_v<T>);
  }
  [[nodiscard]] auto //
  flip_coin() -> bool {
    thread_local independent_bits_engine<default_random_engine, 1, uint_fast8_t>
        coin{};
    coin.seed(random_device{}());
    return coin();
  }

  // NOLINTNEXTLINE: it's fucking template header
  template <size_t n = 1024, class... Args, invocable<Args...> Fx>
  [[nodiscard]] auto                 //
  benchmark(Fx &&fx, Args &&...args) //
      -> nanoseconds {
    const auto epoch = now();
    for (size_t i{}; i != n; ++i) {
      invoke(self_forward(fx), self_forward(args)...);
    }
    return (now() - epoch) / n;
  }
  template <size_t precision = 1024, class... Args, invocable<Args...> Fx>
  [[nodiscard]] auto                          //
  benchmark_to_death(Fx &&fx, Args &&...args) //
      -> nanoseconds {
    nanoseconds new_average{}; // clang-format off
    for (auto [total, times, old_average, same_in_row] =
         tuple{  0ns,   0uz,         0ns,         0uz}; 
         same_in_row != precision;
         total += benchmark(self_forward(fx), self_forward(args)...),
         old_average = exchange(new_average, total / ++times)) { // clang-format on
      if (old_average == new_average) {
        ++same_in_row;
      } else {
        same_in_row = 0;
      }
    }
    return new_average;
  }

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
    //     for (auto const&[index, digits] : toSortCopy)
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
namespace std {
template <class T>
struct hash<boil::vantage_ptr<T>> {
  size_t operator()(const boil::vantage_ptr<T> &arg) const {
    return std::hash<typename boil::vantage_ptr<T>::pointer>{}(to_address(arg));
  }
};
} // namespace std
namespace boil { // bitvector imple________________________________________
template <class Void> template <size_t N> constexpr bitvector<Void>:: //
    bitvector(const bitset<N> &rhs) {
  this->reserve(rhs.size());
  generate_n(back_inserter(*this), rhs.size(),
             [&rhs, i{-1z}] mutable { return rhs[++i]; });
}

template <class Void> template <size_t N> constexpr bitvector<Void>:: //
operator bitset<N>(this const bitvector & self) {
  Expects((N >= self.size()));
  return bitset<N>{self | v::transform([](auto _) { return '0' + _; }) |
                   v::reverse | r::to<string>()};
}

template <class Void> constexpr bool bitvector<Void>:: //
    test(this const bitvector &self, const index bit) {
  Expects((bit < self.size()));
  return self[bit];
}

template <class Void> constexpr bool bitvector<Void>:: //
    all(this const bitvector &self) {
  return r::all_of(self, identity{});
}

template <class Void> constexpr bool bitvector<Void>:: //
    any(this const bitvector &self) {
  return r::any_of(self, identity{});
}

template <class Void> constexpr bool bitvector<Void>:: //
    none(this const bitvector &self) {
  return r::none_of(self, identity{});
}

template <class Void> constexpr size_t bitvector<Void>:: //
    count(this const bitvector &self) {
  return r::count(self, true);
}

template <class Void> bitvector<Void> &bitvector<Void>:: //
operator&=(this bitvector &self, const bitvector &other) {
  r::transform(self, other, self.begin(),
               [](auto &&lhs, auto &&rhs) { return lhs && rhs; });
  return self;
}

template <class Void> bitvector<Void> &bitvector<Void>:: //
operator|=(this bitvector &self, const bitvector &other) {
  r::transform(self, other, self.begin(),
               [](auto &&lhs, auto &&rhs) { return lhs || rhs; });
  return self;
}

template <class Void> bitvector<Void> &bitvector<Void>:: //
operator^=(this bitvector &self, const bitvector &other) {
  r::transform(self, other, self.begin(),
               [](auto &&lhs, auto &&rhs) { return lhs != rhs; });
  return self;
}

template <class Void> bitvector<Void> &bitvector<Void>:: //
operator<<=(this bitvector &self, const size_t shift) {
  shift_left(self.begin(), self.end(), shift);
  r::fill_n(self.rbegin(), shift, false);
  return self;
}

template <class Void> constexpr bitvector<Void> bitvector<Void>:: //
operator<<(this bitvector self, const size_t shift) {
  return self <<= shift;
}

template <class Void> bitvector<Void> &bitvector<Void>:: //
operator>>=(this bitvector &self, const size_t shift) {
  shift_right(self.begin(), self.end(), shift);
  r::fill_n(self.begin(), shift, false);
  return self;
}

template <class Void> constexpr bitvector<Void> bitvector<Void>:: //
operator>>(this bitvector self, const size_t shift) {
  return self >>= shift;
}

template <class Void> constexpr bitvector<Void> bitvector<Void>:: //
operator~(this bitvector self) {
  self.flip();
  return self;
}

template <class Void> bitvector<Void> &bitvector<Void>:: //
    set(this bitvector &self) {
  r::fill(self, true);
  return self;
}

template <class Void> bitvector<Void> &bitvector<Void>:: //
    set(this bitvector &self, const index bit, bool value) {
  self[bit] = value;
  return self;
}

template <class Void> bitvector<Void> &bitvector<Void>:: //
    reset(this bitvector &self) {
  self.clear();
  return self;
}

template <class Void> bitvector<Void> &bitvector<Void>:: //
    reset(this bitvector &self, const index bit) {
  self[bit] = false;
  return self;
}

template <class Void> bitvector<Void> &bitvector<Void>:: //
    flip(this bitvector &self, const index bit) {
  self[bit].flip();
  return self;
}
} // namespace boil

module :private;
// please, don't ask anything about the following...