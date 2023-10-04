#pragma once

#define VERSION_MAJOR 5
#define VERSION_MINOR 0
#define VERSION_SUB 45

#define STR_HELPER(x) #x
#define STR(x) STR_HELPER(x)

#define SERVER_STRING                                                  \
    "FITSWEBQLSE v" STR(VERSION_MAJOR) "." STR(VERSION_MINOR) "." STR( \
        VERSION_SUB)

#define WASM_VERSION "23.10.03.0"
#define VERSION_STRING "F/SV2023-10-04.0-BETA"