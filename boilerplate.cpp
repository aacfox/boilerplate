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
		template <class Container> class pusher {
		public:
			explicit pusher(Container& x) noexcept: _container(&x) {}
			using difference_type = std::ptrdiff_t;
			pusher<Container>& operator= (
				typename Container::const_reference& value) {
				_container->push(value); return *this;
			}
			pusher<Container>& operator= (
				typename Container::value_type&& value) {
				_container->push(std::move(value)); return *this;
			}
			pusher<Container>& operator*() noexcept { return *this; }
			pusher<Container>& operator++() noexcept { return *this; }
			pusher<Container> operator++ (int) { return *this; }
		private:
			Container* _container;
		};

		template<arithmetic Weight = bool> class Graph {
		public:
			struct Edge {
				size_t origin{};
				size_t exit{};
				Weight weight{ 1 };
				bool operator==(const Edge&) const = default;
				auto operator<=>(const Edge& other) const { return weight <=> other.weight; }
			};
		private:
			size_t _fresh_id{ max_v<size_t> };
			vector<Edge> _edge_list;
		public:
			Graph() = default;
			Graph(auto&& edges): _edge_list{ forward_like<decltype(_edge_list)>(edges) } {
				_fresh_id = ranges::max(_edge_list, {}, [](cauto& x) {return max(x.origin, x.exit); });
			}
			template<ranges::range Grid, class Pred = ranges::equal_to> requires
				requires (Grid grid, Pred pred) {
				ranges::begin(grid[0]);
				{ pred(grid[0][0], grid[0][0]) } -> convertible_to<bool>;
			}
			static Graph from_grid(Grid&& grid, Pred pred = {}) {
				Graph graph;
				size_t max_id{};
				for (size_t x{}; x != grid.size(); ++x) {
					for (size_t y{}, width{ grid[0].size() }; y != width; ++y) {
						auto id = [&width, &max_id](cauto i, cauto j) {
							max_id = max(i * width + j, max_id);
							return i * width + j; };
						cauto origin{ id(x, y) };
						if (cauto exit{ id(x + 1, y) }; x + 1 != grid.size()) {
							if (pred(grid[x][y], grid[x + 1][y]))
								graph._edge_list.emplace_back(origin, exit);
							if (pred(grid[x + 1][y], grid[x][y]))
								graph._edge_list.emplace_back(exit, origin);
						}
						if (cauto exit{ id(x, y + 1) }; y + 1 != grid[0].size()) {
							if (pred(grid[x][y], grid[x][y + 1]))
								graph._edge_list.emplace_back(origin, exit);
							if (pred(grid[x][y + 1], grid[x][y]))
								graph._edge_list.emplace_back(exit, origin);
						}
					}
				}
				graph._fresh_id = move(max_id);
				return graph;
			}
			template<class T> requires derived_from<remove_cvref_t<T>, istream>
			static Graph from_csv(T&& input) {
				Graph graph;
				size_t max_id{};
				while (input.good()) {
					Weight weight{}; auto origin{ max_id }, exit{ max_id };
					input >> origin;
					input.ignore(max_v<streamsize>, ';');
					input >> exit;
					input.ignore(max_v<streamsize>, ';');
					input >> weight;
					input.ignore(max_v<streamsize>, '\n');
					max_id = max(max_id, origin);
					graph._edge_list.emplace_back(move(origin), move(exit), move(weight));
				}
				graph._fresh_id = move(max_id);
				return graph;
			}
			auto begin() noexcept { return _edge_list.begin(); }
			auto cbegin() const noexcept { return _edge_list.cbegin(); }
			auto end() noexcept { return _edge_list.end(); }
			auto cend() const noexcept { return _edge_list.cend(); }
			auto edge_count() const noexcept { return _edge_list.size(); }
			auto vertex_count() const noexcept { return _fresh_id + 1; }
			template <size_t Maximum_vertices = 1024> Graph mst() const;
		};

		class Combination {
		public:
			Combination(size_t, size_t);
			template<ranges::sized_range R>
			Combination(R range, size_t k): Combination(range.size(), k) {}
			span<const size_t> operator*() { return _pointers; }
			Combination& operator++();
			bool ok();
		private:
			void restore();
			const size_t _n;
			const size_t _k;
			vector<size_t> _pointers;
			bool _ok{ true };
		};
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

template<arithmetic W> template<size_t N>
Graph<W> Graph<W>::mst() const {
	using Supernode = bitset<N>;
	using edge_set = unordered_set < Edge, decltype([](cauto& edge) noexcept {
		return hash<size_t>{}(edge.origin) ^ hash<size_t>{}(edge.exit); }),
		decltype([](cauto& x, cauto& y) {
		if (x.weight == y.weight and (
			x.origin == y.origin and x.exit == y.exit or
			x.exit == y.origin and y.exit == x.origin))
			return true; else return false; }) > ;
	//TL;DR above: graph is interpreted as undirected
	using edge_multimap = unordered_multimap<Supernode, Edge>;
	if (N < vertex_count()) throw (length_error("Default template argument for number of vertices (which is 1024) proved insufficient."));
	edge_multimap edges; //bitset-based hash map for easier node combining
	vector<Supernode> supernodes(vertex_count()); //vector for tracking remaining supernodes
	vector nodes(vertex_count(), supernodes.end()); //disjoint set of original nodes
	//the latter two can be interpreted as nonoptimized-memory-wise hash_maps 
	//vertice_ID -> respective_type
	for (cauto edge : _edge_list) {
		supernodes[edge.origin].set(edge.origin);
		nodes[edge.origin] = supernodes.begin() + edge.origin;
		edges.emplace(supernodes[edge.origin], move(edge));
	}
	//this is one of the subroutines of boruvka(); should be used once
	//on the original graph, incase it contains self loops or redundant edges
	auto iter{ edges.cend() }, sentinel{ iter };
	auto trim_selfs_redundants = [&](edge_multimap& some_edges) {
		for (unordered_map<Supernode, decltype(iter)> connections;
		  cauto & supernode : supernodes
		  | views::filter([](cauto& x) { return x.any(); }))
			for (tie(iter, sentinel) = some_edges.equal_range(supernode);
				  iter != sentinel;) {
			if (supernode[iter->second.exit]) { //i.e. a self-loop
				some_edges.erase(iter++);
				continue;
			} //else check if the edge's redundant. For that,
			// firstly, check if the connection wasn't considered before yet or was already.
			if (auto connected{ connections.find(*nodes[iter->second.exit]) };
				 connected == connections.cend())//first case
				connections[supernode] = iter++;
			else if (iter->second.weight < connected->second->second.weight)
				some_edges.erase(exchange(connected->second, iter++));
			else some_edges.erase(iter++);
			connections.clear();
		}
	};
	trim_selfs_redundants(edges);
	//The actual Randomized Expected Linear-time MST algorithm:
	auto rmst = [&](this auto self, edge_multimap& input) {
		edge_set boruvkas;
		auto boruvka = [&]() {
			edge_set buffer;
			//choosing-incident-edges cycle:
			for (auto lightest{ iter }; cauto & node : supernodes
				  | views::filter([](cauto& x) { return x.any(); })) {
				tie(iter, sentinel) = input.equal_range(node);
				lightest = ranges::min_element(iter, sentinel, [](cauto& x, cauto& y) {
					return x.second.weight < y.second.weight; });
				if (lightest != sentinel) buffer.insert(input.extract(lightest).mapped());
			}
			//contracting cycle:
			for (auto absorbing{ supernodes.end() }, absorbed{ absorbing };
				  cauto & edge : buffer) {
				absorbing = nodes[edge.origin];
				absorbed = nodes[edge.exit];
				if (auto [combined, changing] = tuple{ *absorbing | *absorbed,
					 edge_multimap{} }; absorbing != absorbed) {
					auto update_keys = [&](cauto& node) {
						for (tie(iter, sentinel) = input.equal_range(node); iter != sentinel;) {
							auto temp{ input.extract(iter++) };
							temp.key() = combined;
							changing.insert(move(temp));
						}
					};
					update_keys(*absorbing);
					update_keys(*absorbed);
					input.merge(changing);
					for (size_t i{}; i != vertex_count(); ++i)
						if ((*absorbed)[i]) nodes[i] = absorbing;
					*absorbing = move(combined);
					absorbed->reset();
				}
			}
			boruvkas.merge(buffer);
			trim_selfs_redundants(input);
		};
		auto erase_f_heavies = [&](const edge_set& forest) { //probable signature
			//perform some magic modificated linear time mst verification algo 
			//for deleting F_heavy edges from contracted graph,
			//given the forest of its subgraph
		};
		if (input.empty()) return edge_set{};
		boruvka();
		boruvka();
		edge_multimap subgraph;
		subgraph.reserve(input.bucket_count()); //to prevent rehashing
		for (auto yes_no{ randomizer(0, 1) }; cauto edge : input)
			if (yes_no()) subgraph.emplace(move(edge));
		erase_f_heavies(self(subgraph));
		boruvkas.merge(self(input));
		return boruvkas;
	};
	edge_set mst_edges{ rmst(edges) };
	Graph mst; //time to restore invariants:
	mst._fresh_id = _fresh_id;
	for (mst._edge_list.reserve(2 * mst_edges.size());
		  auto & edge: mst_edges) {
		mst._edge_list.emplace_back(edge.exit, edge.origin, edge.weight);
		mst._edge_list.emplace_back(move(edge));
	}
	return mst;
}

module: private; //complete types' implementations:

Combination::Combination(size_t n, size_t k): _n{ n }, _k{ k } {
	if (_n < _k)
		throw invalid_argument("N should be greater or equal to K.");
	_pointers.resize(_k);
	if (_k == 0) _ok = false;
	ranges::iota(_pointers, 0);
}
Combination& Combination::operator++() {
	for (size_t forwarded{}; auto & i : _pointers | views::reverse) {
		if (i + 1 != _n - forwarded) break;
		else {
			i = 0;
			++forwarded;
		}
	}
	restore();
	return *this;
}
void Combination::restore() {
	for (auto [i, j] : _pointers | views::adjacent<2>)
		if (i >= j) j = i + 1;
}
bool Combination::ok() {
	cauto old{ _ok };
	if (_pointers[0] == _n - _k)
		_ok = false;
	return old;

}

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