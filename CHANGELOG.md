## [5.0.56] - 2024-02-24

* set the ZeroMQ port to 0 by default, hence assuming a single-server mode (the port can be specified in the _*config.ini*_ file)
* corrected the P-V Diagram line length calculation inside a FORTRAN subroutine, added capacity checks to prevent potential out-of-bounds accesses
* shifted handling of <submit_progress> HTTP POST requests from _*mongoose*_ to _*libmicrohttpd_* 

## [5.0.55] - 2024-02-22

* added a 120s timeout to libmicrohttpd start-up flags, removed MHD_USE_TURBO
* replaced HTML5 Canvas with WebGL for streaming video frames
* removed the _*optional*_ mongoose HTTP client part (using libcurl instead)
* added a CHANGELOG popup to the _*Help*_ menu
* completed the multi-dataset support

## [5.0.54] - 2024-01-29

* corrected a problem with spectral lines (molecules) not being displayed after modifying _*V_SRC*_ and/or clicking _*F_REST*_ in a web browser
* updated the server-side _*mongoose*_ C networking library and replaced a UDP wake-up helper with a new _*mg_wakeup()*_ function from _*mongoose*_
* the server-side WebSockets: separated the event loops for real-time spectra/video and PV-Diagram WebSocket messages
* .tar.gz FUGIN multiple-dataset partial FITS downloads compressed with a microtar ANSI C library
* added handling of HTTP 204 replies to the client-side JavaScript _*fetch_image_spectrum()*_ function

## [5.0.53] - 2023-12-08

* updated the server-side _*mongoose*_ C networking library
* improved the responsiveness of the server-side WebSockets mongoose event loop (added a UDP wake-up helper)
* worked around a potential double _*free()*_ issue (under heavy stress duplicate WebSocket messages could have been retrieved from the mongoose queue, leading to extra _*free()*_ invocations)

## [5.0.52] - 2023-12-01

* disabled the _*TCMALLOC*_ memory allocator (a suspected bug in _*tcmalloc*_ small memory allocations caused a segmentation fault)
* alternatively a _*buggy*_ x265 library might need replacing by Intel SVT-HEVC; for now reverted the memory allocator back to the standard GNU C _*malloc*_/_*calloc*_/_*realloc*_/_*free*_ whilst testing another allocator: _*mimalloc*_
* recompiled the WebAssembly with the latest emscripten compiler (enabled the _*mimalloc*_ support)

## [5.0.51] - 2023-11-17

* adjusted the client-side JavaScript _*clipSize*_ zoom viewport size variable

## [5.0.50] - 2023-11-15

* replaced the external _*JEMALLOC*_ memory allocator with _*TCMALLOC*_ (a bug in _*jemalloc*_ caused a segmentation fault)
* recompiled the WebAssembly with the latest emscripten compiler

## [5.0.49] - 2023-11-07

* refined the WebGL viewport pixel coordinate translation

## [5.0.48] - 2023-11-01

* improved the translation between the web browser mouse coordinates and the underlying FITS image pixel coordinates
* updated the mongoose C networking library to v7.12

## [5.0.47] - 2023-10-25

* a new functionality: an initial support for importing ds9 region files (*.reg) with _*image*_/_*physical*_/_*wcs*_ (_*fk5*_ only) coordinates and two types of shapes: _*circle*_ and _*point*_ (_*hint*_: a menu setting _*FITS*_ / _*import ds9 region*_)
* incorporated the WCSLIB library server-side (in the CSV export FORTRAN code ) as well as in the client-side JavaScript (calling the WASM-compiled C code), which replaces a custom implementation of the WCS conversion routines and provides a better compatibility with ds9
* a minor fix to the CSV export functionality (fixed the _*RA*_ string parsing in the server C code)

## [5.0.46] - 2023-10-11

* corrected the display of _*RA*_/_*DEC*_ coordinates (for example when moving the mouse over the FITS image), which brings the _*beta*_ v5 into line with FITSWebQL v4 and ds9

## [5.0.45] - 2023-10-04

* an improved P-V diagram: an _*all-new*_ crosshair with a tooltip (_*hint*_: hover a mouse over the P-V diagram)

## [5.0.44] - 2023-09-14

* spectrum binning: replaced the _*Largest Triangle Three Buckets*_ downsampling with true binning (averaging adjacent values)

## [5.0.43] - 2023-09-13

* added a new spectrum binning functionality (see a menu setting _*Preferences*_ / _*spectrum binning*_)

## [5.0.42] - 2023-09-11

* fixed a long-standing "_*disappearing menu*_" issue in Firefox (worked around a Firefox CSS hover bug)

## [5.0.41] - 2023-09-05

* re-worked the client-side image legend WebGL display to get around a bug (a slightly shifted legend position) for some combinations of Safari / macOS versions
* removed _*Google Analytics*_

## [5.0.40] - 2023-08-28

* an improved P-V Diagram: changed the X-axis to display an angular distance (in _*arcseconds*_) instead of a P-V line pixel number

## [5.0.39] - 2023-08-21

* corrected a velocity axis direction in the P-V Diagram for datasets with a reversed frequency / velocity axis
* added a user-moveable horizontal velocity reference line at _*v=0*_ (mid-velocity if there is no zero) to the P-V Diagram

## [5.0.38] - 2023-08-08

* corrected the auto-detection of radio FITS files (the previous version would incorrectly detect them as optical) for the newly added NRO45M datasets, based on the FITS header telescope keyword

## [5.0.37] - 2023-07-31

* updated the client/server ZFP compression library to v1.0.0
* started loading the client-side JavaScript library _*d3.js*_ from the global Content Delivery Network (CDN)
* added support for new _*NRO45M*_ datasets
* recompiled WebAssembly with the latest _*emscripten*_ compiler
* on-going work on the NOBEYAMA FUGIN composite RGB support

## [5.0.36] - 2023-05-30

* updated the mongoose C networking library to v7.10
* re-worked internal messaging to take advantage of the new mongoose v7.10 lock-free message queue (_*improved performance*_)
* applied glib atomic reference counting to the user session C structure (_*improved reliability*_)

## [5.0.35] - 2023-05-24

* Introduced a hard limit of 500 Splatalogue spectral lines that can be displayed simultaneously in the website. This has been done in order to prevent an information overload and a website slowdown. The 500 lines that are displayed will be chosen as a random subset of the larger than 500 original number of lines. Please refine your search criteria to avoid a hard limit. If the hard limit has been reached information will be displayed in the web browser JavaScript console (press F12 to view it in most browsers).
* On-going work on NOBEYAMA FUGIN

## [5.0.34] - 2023-04-25

* overhauled the server logging functionality (moved from FORTRAN to C)
* ensured compatibility with the new gcc / gfortran v13
* updated _*http.c*_ so that it compiles cleanly with the latest glib _*g_string_free(..., FALSE)*_ function

## [5.0.33] - 2023-04-21

* speeded up initial web page loading by using _*async*_ / _*await*_ within JavaScript
* switched loading of the 3D surface script from the local server over to the global CDN (Content Delivery Network)
* speeded up loading of contour lines _*new Worker(...)*_ scripts
* on-going work on the NOBEYAMA FUGIN composite RGB support

## [5.0.32] - 2023-04-13

* client-side JavaScript: improved WebGL reliability

## [5.0.31] - 2023-04-06

* replaced the ```html_encode()``` C function with a more robust ```mg_url_encode()```
* added initial support for loading multiple (composite) FITS files from the NOBEYAMA FUGIN project across the cluster
* added support for multiple (composite) FITS files to the WebSocket user session C structure
* propagating WebSocket heartbeats across the cluster using HTTP messages 

## [5.0.30] - 2023-03-29

* added compressed FITS files support to the external URL file downloads (_*.fits.Z_, _*.fits.gz_, _*.fits.zip_, _*.fits.bz2_)
* font colour adjustment in the P-V Diagram _#contour levels_ input field
* shortened the _source code_ entry in the web browser welcome screen

## [5.0.29] - 2023-03-15

* re-worked the client-side JavaScript P-V Diagram event loop to reduce the load on the server

## [5.0.28] - 2023-03-15

* improved the colourmap in the P-V Diagram (removed the _erf()_ scaling step)
* pixel values are now simply standardised / capped to the range [-3,3] prior to scaling to [0,1] for the colourmap
* adjusted the font colour in the P-V Diagram _#contour levels_ input field

## [5.0.27] - 2023-03-10

* adjusted the font family used in the welcome screen _CHANGELOG_

## [5.0.26] - 2023-03-10

* added a _CHANGELOG_ to the web browser welcome screen

## [5.0.25] - 2023-03-08

Added the ability to change the number of contour level as well as a choice of three contour-level scaling functions to the P-V Diagram:
* _linear_ (default)
* _sqrt_
* _logarithmic_
    
## [5.0.24] - 2023-03-06

* added contouring (a _linear_ scale only at the moment) to the P-V Diagram with a corresponding checkbox to enable/disable contouring
* improved serving the WASM files via the CDN (added the release numbers for better version control)
    
## [5.0.23] - 2023-03-02

* fixes the problem with cross-origin reading of CSS rules when the CSS file is being served via the global Content Delivery Network (CDN)
* this problem prevented proper styling of the UI themes
* instead of reading / removing CSS rules (prohibited by CORS), the rules needed by the dark theme are now being added dynamically from within JavaScript
    
## [5.0.22] - 2023-03-02

* Upon a sub-region selection, if a tone mapping function has changed the image would not have been displayed correctly. This has been fixed by refreshing the tone mapping from within the client-side JavaScript.
    
## [5.0.21] - 2023-03-01

* Forces the GitHub release package to contain the latest ```fitswebqlse.js``` version.
    
## [5.0.20] - 2023-02-27

* Changed the default colourmap from "amber" back to "green". "green" (default in v4) has been in constant use since ALMAWebQL v2. Users are free to change at any time the colourmap persistent setting from the menu.
* Serving of fitswebqlse.js and fitswebqlse.css files has been switched over to the global CDN.
    
## [5.0.19] - 2023-02-27

* web browser welcome screen: added links to "User Guide" sections on the P-V Diagram as well as CSV Export functionality
* added a short video to the "User Guide" showing how to use the new interactive P-V Diagram
    
## [5.0.18] - 2023-02-24

* Set the value of ```MG_DATA_SIZE``` to 50 bytes (the original value used by mongoose v7.8) in the Makefile (```MG_DATA_SIZE``` is used internally by the recently upgraded C mongoose library v7.9). This fixes the issue with the P-V Diagram not displaying in the web browser.
    
## [5.0.17] - 2023-02-22

* upgraded the internal mongoose C networking library from v7.8 to v7.9
* client-side P-V Diagram: made it easy to cancel the P-V diagram line selection by moving the mouse outside of the main FITS image
* server-side: fixed a segmentation fault when the two different FORTRAN median estimation subroutines were given one-element arrays
* small client-side JavaScript adjustments (the P-V Diagram contouring has been disabled for now until an improved version is created)
    
## [5.0.16] - 2023-02-17

* Added support for opening / downloading FITS files with all BITPIX values, with one caveat: ```float64``` values that cannot be coerced into the ```float32``` range will result in an error. The full-range ```float64``` support will be enabled later on with the internal pixel handling switched from ```float32``` to a hybrid ```float32/64``` one
* Improved parsing of FITS headers during external URL downloads
* A minor bug fix in handling of incomplete datasetid URLs at JVO
    
## [5.0.15] - 2023-02-09

* improved handling of NFS mount failures across the cluster (introduced the ```loading_mtx``` to FITS ```read / load / delete``` subroutines)
* added ```sleep(1)``` / retries to deal with failed ```curl_easy_perform()``` calls under heavy network load
* added a ```try lock()``` on the ```loading_mtx``` in the ```notify_root``` FORTRAN subroutine
    
## [5.0.14] - 2023-02-03

Re-done opening FITS files from external URLs. The new code parses / processes the FITS files on the fly whilst they are being downloaded, just as the previous FITSWebQL v3 (C/C++) and v4 (Rust) versions used to do.
    
## [5.0.13] - 2023-01-19

* fixes premature timeouts for 2D (image-only) FITS datasets when opened in a cluster environment
    
## [5.0.12] - 2023-01-16

* added a preliminary cluster-aware external URL support for loading FITS files (a more efficient solution is being worked on)
* a small bug fix (uninitialized variable)
    
## [5.0.11] - 2023-01-11

* real-time contouring in streaming video frames (when the "contours" checkbox is ON)
* contouring of the P-V Diagram
* upgraded Google Analytics to version 4
    
## [v5.0.10] - 2022-12-12

* a new feature: realtime P-V Diagram
    
## [v5.0.9] - 2022-09-06

Probably (hopefully) the last adjustment targeting macOS Homebrew.
    
## [v5.0.8] - 2022-09-05

More adjustments to accommodate Homebrew: this time passing the location of the Splatalogue SQLite file and the "htdocs" directory.
    
## [v5.0.7] - 2022-09-02

* macOS: adjusted the gcc/gfortran compiler path so that a Homebrew formula uses gcc/gfortran from the Homebrew repository instead of Apple's clang, which that does not contain OpenMP support.
    
## [v5.0.6] - 2022-09-02

* macOS: adjusted the Makefile in order to improve setting the Homebrew prefix (a fix needed when compiling from within a Homebrew formulae) 
* added a ```MAGIC_NUMBER``` to the binary cache file in order to handle automatically any cache file format changes (the cache directory content will now get invalidated automatically)
    
## [v5.0.5] - 2022-09-01

* Another binary cache disk format change: removed the redundant ```item%uri``` field from ```{save, load}_dataset()```. ```item%uri``` is set to a new value each time upon opening a FITS file.
* Added a staging step ```NFS-->glusterfs``` to the preload fitsDB.jl Julia program. 
    
## [v5.0.4] - 2022-08-31

* Switched over to the Content Delivery Network (CDN) for the WASM files.
* Added the missed ```create_root_path()``` functionality for the server edition.
* The binary cache disk format has changed: the storage switched from ```real(kind=4)``` to ```real(kind=8)``` for the floating-point fields read from the FITS header. This was done in order to improve the precision of frequency/velocity calculations.
    
## [v5.0.3] - 2022-08-30

* recompiled WebAssembly with the latest emscripten
* switched to proper versioning with specific dates instead of XX.X
* a minor bug fix
    
## [v5.0.2] - 2022-08-25

* replaced fortran-unix pthread bindings by custom FORTRAN bindings (via a custom C bridge) to POSIX threads that work properly on macOS as well as Linux
* added multi-threaded "tileLanczos" and "tileSuper" image downsizing functions using Intel IPP (Linux only, macOS uses Apple Accelerate instead)
    
## [v5.0.1] - 2022-08-24

* Speed improvements in image downsizing.
* Switched to Apple Accelerate vImage on Intel macOS too (not just on Apple Silicon).
* Improved support for large-resolution FITS images / cubes (for example ```width = 30000, height = 20000```).

## v5.0.0 - 2022-08-17

* optimised native Apple Silicon support (using macOS Accelerate vImage for downsizing images)
* for now only a single-dataset view is supported
