// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include <unordered_map>

#if !defined(__APPLE__)
#include <memory_resource>
#ifndef WF_USE_PMR_MAP
#define WF_USE_PMR_MAP
#endif  // WF_USE_PMR_MAP
#endif  // !__APPLE__

// This header is just so we disable use of PMR on OSX, where it isn't supported until OSX14:
// See related error in qt: https://bugreports.qt.io/browse/QTBUG-114316
namespace wf {

// Map type used by `addition` and `multiplication`.
#ifdef WF_USE_PMR_MAP
template <class K, class V, class H, class E>
using stl_pmr_unordered_map = std::pmr::unordered_map<K, V, H, E>;
#else
template <class K, class V, class H, class E>
using stl_pmr_unordered_map = std::unordered_map<K, V, H, E>;
#endif  // WF_USE_PMR_MAP

}  // namespace wf
