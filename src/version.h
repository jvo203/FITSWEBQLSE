#pragma once

#define VERSION_MAJOR 5
#define VERSION_MINOR 1
#define VERSION_SUB 15

#define STR_HELPER(x) #x
#define STR(x) STR_HELPER(x)

#define SERVER_STRING                                                  \
    "FITSWEBQLSE v" STR(VERSION_MAJOR) "." STR(VERSION_MINOR) "." STR( \
        VERSION_SUB)

#define WASM_VERSION "24.09.17.0"
#define VERSION_STRING "F/SV2024-12-05.1"