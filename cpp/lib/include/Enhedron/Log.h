//
//          Copyright Simon Bourne 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#include "Log/DefaultWriter.h"
#include "Log/Logger.h"

namespace Enhedron {
    using Logger = Log::ConfigurableLogger<Log::DefaultWriter>;
}
