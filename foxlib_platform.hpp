#include "foxlib.hpp"
#if defined(_WIN32) || defined(_WIN64)
#    include "foxlib_windows.hpp"
#else
#    error Only windows is currently supported.
#endif