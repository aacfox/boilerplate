module;
#define cauto const auto
#include <execution>
#include <gsl/gsl>
export module boilerplate:boil;
export import std;
export {
  using namespace std;
  using namespace chrono;
  using namespace filesystem;
  using namespace execution;
  namespace r = ranges;
  namespace v = views;
  using gsl::narrow, gsl::narrow_cast, gsl::at, ranges::forward_range,
      ranges::range, ranges::borrowed_range, ranges::random_access_range,
      ranges::sized_range, ranges::common_range, ranges::sized_range,
      ranges::view;

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
  enum class order : signed char {
    equal = 0,
    equivalent = equal,
    less = -1,
    greater = 1,
    unordered = -128,
  };
  constexpr auto now = &high_resolution_clock::now;
  constexpr auto today() noexcept {
    return year_month_day{floor<days>(system_clock::now())};
  }
  constexpr auto o_clock() noexcept {
    return hh_mm_ss{floor<seconds>(system_clock::now()) -
                    floor<days>(system_clock::now())};
  }
  constexpr auto subrange(pair_like auto pair) noexcept {
    static_assert(sentinel_for<std::tuple_element_t<1, decltype(pair)>,
                               std::tuple_element_t<0, decltype(pair)>>,
                  "Denotes invalid subrange!");
    return r::subrange{get<0>(pair), get<1>(pair)};
  }
  constexpr void create_file(convertible_to<path> auto &&filename) noexcept {
    ofstream{filename};
  }
  } // namespace utilities

  inline namespace classes {
  class Exception : public exception {
    // clang-format off
  public:
    Exception(string_view what = "Unknown exception.",
              source_location where = source_location::current()
              #ifdef __cpp_lib_stacktrace
              , stacktrace when = stacktrace::current(1)
              #endif
             ): _what{std::move(what)}, _where{std::move(where)}
              #ifdef __cpp_lib_stacktrace
                ,_when{std::move(when)}
              #endif
    {}
    virtual const char *what() const noexcept { return _what.data(); }
    virtual const source_location &where() const noexcept { return _where; }
    #ifdef __cpp_lib_stacktrace
    virtual const stacktrace &when() const noexcept { return _when; }
    #endif

  private:
    string_view _what;
    source_location _where;
    #ifdef __cpp_lib_stacktrace
    stacktrace _when;
    #endif
    // clang-format on
  };
  } // namespace classes

  inline namespace functions {
  template <arithmetic T> auto randomizer(T min = min_v<T>, T max = max_v<T>) {
    random_device seeder;
    default_random_engine generator{seeder()};
    if constexpr (is_floating_point<T>())
      return bind(uniform_real_distribution{min, max}, generator);
    else if constexpr (is_integral<T>())
      return bind(uniform_int_distribution{min, max}, generator);
  }

  template <class... Args, invocable<Args...> Fx>
  auto benchmark(ostream &output, Fx &&func, Args &&...args) {
    constexpr size_t iters{1000};
    auto const epoch = now();
    for (size_t i{}; i != iters; ++i)
      invoke(forward<Fx>(func), forward<Args>(args)...);
    auto const delta{now() - epoch};
    println(output, "Benchmark completed with {}ns.", delta.count() / iters);
    return delta;
  }
  } // namespace functions
  } // namespace boil
}

// Don't ask anything about the following... Just ignore it.
// using gsl::narrow, gsl::narrow_cast,
// std::abs, std::array, std::atomic,
// std::back_inserter,
// std::cerr, std::cin, std::cout,
// std::default_random_engine,
// std::endl, std::erase, std::erase_if,
// std::floating_point, std::floor, std::forward, std::front_inserter,
// std::fstream, std::function, std::getline, std::identity, std::ifstream,
// std::inserter, std::integral, std::invocable, std::invalid_argument,
// std::invoke, std::ios, std::istream, std::map, std::numeric_limits,
// std::ofstream, std::optional, std::ostream, std::output_iterator,
// std::pair, std::pow, std::print, std::println, std::projected,
// std::priority_queue, std::queue, std::random_device, std::ref,
// std::reference_wrapper, std::set, std::span, std::sqrt, std::stack,
// std::string, std::string_view, std::tuple, std::tuple_size,
// //std::this_thread, std::uniform_int_distribution,
// std::uniform_real_distribution, std::unordered_map, std::vector;