// Copyright (C) 2014 Enhedron Ltd


#pragma once

// This file shouldn't be included in pre compiled headers.

#include "Log/DefaultWriter.h"
#include "Log/Logger.h"

namespace Enhedron {
	using Logger = Log::ConfigurableLogger<Log::DefaultWriter>;
}
