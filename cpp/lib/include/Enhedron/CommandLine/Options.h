#include <string>

namespace Enhedron { namespace CommandLine { namespace Impl { namespace Impl_Options {
    using std::string;
    class Help {};

    enum class ExitStatus {
        OK
    };

    template<typename Functor>
    void readCommandLine(string description, int argc, const char* const argv[], Functor&& functor) {
        functor(Help{});
    }
}}}}

namespace Enhedron { namespace CommandLine {
    using Impl::Impl_Options::ExitStatus;
    using Impl::Impl_Options::Help;
    using Impl::Impl_Options::readCommandLine;
}}
