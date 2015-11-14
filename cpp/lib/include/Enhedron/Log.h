// Copyright (C) 2014 Simon Bourne

#pragma once

#include "Log/DefaultWriter.h"
#include "Log/Logger.h"

namespace Enhedron {
    using Logger = Log::ConfigurableLogger<Log::DefaultWriter>;
}
