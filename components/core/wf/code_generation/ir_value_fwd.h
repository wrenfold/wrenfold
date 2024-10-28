// wrenfold symbolic code generator.
// Copyright (c) 2024 Gareth Cross
// For license information refer to accompanying LICENSE file.
#pragma once
#include "wf/utility/checked_pointers.h"

namespace wf::ir {

class block;
class value;
using block_ptr = non_null<ir::block*>;
using value_ptr = non_null<ir::value*>;
using const_block_ptr = non_null<const ir::block*>;
using const_value_ptr = non_null<const ir::value*>;

}  // namespace wf::ir
