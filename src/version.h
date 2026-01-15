#pragma once

#define VERSION_MAJOR 5
#define VERSION_MINOR 1
#define VERSION_SUB 17

#define STR_HELPER(x) #x
#define STR(x) STR_HELPER(x)

#define SERVER_STRING                                                  \
    "FITSWEBQLSE v" STR(VERSION_MAJOR) "." STR(VERSION_MINOR) "." STR( \
        VERSION_SUB)

#define WASM_VERSION "26.01.13.0"
#define VERSION_STRING "F/SV2026-01-15.0"