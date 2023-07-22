#pragma once

#define VERSION_MAJOR 5
#define VERSION_MINOR 0
#define VERSION_SUB 36

#define STR_HELPER(x) #x
#define STR(x) STR_HELPER(x)

#define SERVER_STRING                                                  \
    "FITSWEBQLSE v" STR(VERSION_MAJOR) "." STR(VERSION_MINOR) "." STR( \
        VERSION_SUB)

#define WASM_VERSION "23.06.30.0"
#define VERSION_STRING "F/SV2023-07-22.0-BETA"