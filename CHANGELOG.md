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