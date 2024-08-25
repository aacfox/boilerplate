module;
#define cauto const auto
export module boilerplate;
export import std;
export import <gsl/gsl>;
export {
	using namespace std;
	using namespace std::chrono;
	using namespace std::filesystem;
	using namespace std::execution;
	using gsl::narrow, gsl::narrow_cast, gsl::at;
}

export namespace boil {
	inline namespace utilities {
		enum class order: signed char {
			equal = 0,
			equivalent = equal,
			less = -1,
			greater = 1,
			unordered = -128,
		};
		constexpr auto now = &high_resolution_clock::now;
		template <class N> concept arithmetic = floating_point<N> or integral<N>;
		template <arithmetic T> constexpr auto max_v = numeric_limits<T>::max();
		template <arithmetic T> constexpr auto min_v = numeric_limits<T>::min();
		auto today() noexcept {
			return year_month_day{ floor<days>(system_clock::now()) };
		}
		auto o_clock() noexcept {
			return hh_mm_ss{ floor<seconds>(system_clock::now()) - floor<days>(system_clock::now()) };
		}
		auto hash_combine = [](cauto& x, cauto& y) noexcept {
			size_t origin{ hash{}(x.origin) };
			return origin ^ (hash{}(x.exit) + 0x517cc1b727220a95
								+ (origin >> 2) + (origin << 6)); };
		auto subrange(auto pair) { return ranges::subrange{ get<0>(pair),get<1>(pair) }; }
		template<class Path> requires same_as<remove_cvref_t<Path>, path>
		void create_file(Path&& p) { ofstream{ p }; }
	}

	inline namespace classes {
		class Exception: public exception {
		public:
			Exception(string_view what = "Unknown exception.",
						 source_location where = source_location::current(),
						  stacktrace when = stacktrace::current(1)):
				_what{ move(what) }, _where{ move(where) }, _when{ move(when) } {
			}
			const char* what() const noexcept override { return _what.data(); }
			virtual const source_location& where() const noexcept { return _where; }
			virtual const stacktrace& when() const noexcept { return _when; }
		private:
			string _what;
			source_location _where;
			stacktrace _when;
		};

		template<class... Functors>
		struct Visitor: Functors...{ using Functors::operator()...; };
	}

	inline namespace functions {
		template <arithmetic T>
		auto randomizer(T min = min_v<T>, T max = max_v<T>) {
			random_device seeder;
			default_random_engine generator{ seeder() };
			if constexpr (is_floating_point<T>())
				return bind(uniform_real_distribution{ min, max }, generator);
			else if constexpr (is_integral<T>())
				return bind(uniform_int_distribution{ min, max }, generator);
		}

		template <class...Args, invocable<Args...> Fx>
		auto benchmark(ostream& output, Fx&& func, Args&&...args) {
			constexpr size_t iters{ 1000 };
			auto const epoch = now();
			for (size_t i{}; i != iters; ++i)
				invoke(forward<Fx>(func), forward<Args>(args)...);
			auto const delta{ now() - epoch };
			println(output, "Benchmark completed with {}ns.",
					  delta.count() / iters);
			return delta;
		}
	}
}

using namespace boil; //templates' implementations:

module: private; //complete types' implementations:

//Don't ask anything about the following... Just ignore it.
//using gsl::narrow, gsl::narrow_cast,
//std::abs, std::array, std::atomic,
//std::back_inserter,
//std::cerr, std::cin, std::cout,
//std::default_random_engine,
//std::endl, std::erase, std::erase_if,
//std::floating_point, std::floor, std::forward, std::front_inserter, std::fstream, std::function,
//std::getline,
//std::identity, std::ifstream, std::inserter, std::integral, std::invocable, std::invalid_argument, std::invoke, std::ios, std::istream,
//std::map,
//std::numeric_limits,
//std::ofstream, std::optional, std::ostream, std::output_iterator,
//std::pair, std::pow, std::print, std::println, std::projected, std::priority_queue, std::queue,
//std::random_device, std::ref, std::reference_wrapper,
//std::set, std::span, std::sqrt, std::stack, std::string, std::string_view,
//std::tuple, std::tuple_size, //std::this_thread,
//std::uniform_int_distribution, std::uniform_real_distribution, std::unordered_map,
//std::vector;