#pragma once

#define VERSION_MAJOR 5
#define VERSION_MINOR 2
#define VERSION_SUB 1

#define STR_HELPER(x) #x
#define STR(x) STR_HELPER(x)

#define SERVER_STRING                                                  \
    "FITSWEBQLSE v" STR(VERSION_MAJOR) "." STR(VERSION_MINOR) "." STR( \
        VERSION_SUB)

#define WASM_VERSION "26.03.18.0"
#define VERSION_STRING "F/SV2026-03-30.0"