module;
#include <gsl/gsl>
export module boilerplate:radix_sort;
import :boil;
using namespace boil;

// TODO(aacfox): on maintaining

// export template <class T, class Projection = std::identity>
// void radix_sort(std::span<T> span, Projection projection = {},
//                 bool LSD = true) {
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
// }