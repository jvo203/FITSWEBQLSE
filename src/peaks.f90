!> @mainpage
!!
!! @section intro_sec Introduction
!! PEAKS is a peak detection library meant to locate peaks and valleys in a 
!! signal.  This library works on both smooth data and on noisy data where other
!! routines, especially those that rely upon derivatives, have difficulties.
!! This library contains routines that are a re-implementation of the peak
!! detection routine presented by Eli Billauer, which can be found 
!! [here](http://billauer.co.il/peakdet.html).
module peaks
    use iso_fortran_env
    implicit none
    private
    public :: peak_detect

    !> @brief Attempts to locate local maxima (peaks), and local minima 
    !! (valleys) within a signal.
    !!
    !! @par Syntax 1
    !! @code{.f90}
    !! subroutine peak_detect( &
    !!  real(real32) x(:), &
    !!  real(real32) delta, &
    !!  allocatable integer(int32) maxind(:), &
    !!  allocatable real(real32) maxvals(:), &
    !!  allocatable integer(int32) minind(:), &
    !!  allocatable real(real32) minvals(:) &
    !! )
    !! @endcode
    !!
    !! @par Syntax 2
    !! @code{.f90}
    !! subroutine peak_detect( &
    !!  real(real64) x(:), &
    !!  real(real64) delta, &
    !!  allocatable integer(int32) maxind(:), &
    !!  allocatable real(real64) maxvals(:), &
    !!  allocatable integer(int32) minind(:), &
    !!  allocatable real(real64) minvals(:) &
    !! )
    !! @endcode
    !!
    !! @param[in] x An N-element array containing the signal to analyze.
    !! @param[in] delta A threshold value that determines the sensitivity the
    !!  algorithm has to discerning local maxima or minima from noise.
    !! @param[out] maxind An allocatable array that will be allocated by this
    !!  routine and filled with the indices of the local maxima.
    !! @param[out] maxvals An allocatable array that will be allocated by this
    !!  routine and filled with the local maxima.
    !! @param[out] minind An allocatable array that will be allocated by this
    !!  routine and filled with the indices of the local minima.
    !! @param[out] minvals An allocatable array that will be allocated by this
    !!  routine and filled with the local minima.
    !!
    !! @par Example
    !! The following example is the example provided by Eli Billauer on his 
    !! [website](http://billauer.co.il/peakdet.html).  This example also makes
    !! use of the [FPLOT](https://github.com/jchristopherson/fplot) library to 
    !! display the plot.
    !!
    !! @code{.f90}
    !! program example
    !!     use iso_fortran_env
    !!     use peaks
    !!     use fplot_core
    !!     implicit none
    !!
    !!     ! Parameters
    !!     integer(int32), parameter :: npts = 10000
    !!     real(real64), parameter :: dt = 1.0d-3
    !!     real(real64), parameter :: delta = 0.5d0
    !!
    !!     ! Local Variables
    !!     integer(int32), allocatable, dimension(:) :: imx, imn
    !!     real(real64) :: t(npts), x(npts), r(npts)
    !!     real(real64), allocatable, dimension(:) :: mx, mn, tmx, tmn
    !!     type(plot_2d) :: plt
    !!     type(plot_data_2d) :: d1, d2, d3
    !!     class(plot_axis), pointer :: xAxis, yAxis
    !!
    !!     ! Build a signal
    !!     call random_number(r)
    !!     t = linspace(0.0d0, dt * (npts - 1), npts)
    !!     x = 0.3d0 * sin(t) + sin(1.3d0 * t) + 0.9d0 * sin(4.2d0 * t) + 2.0d-1 * r
    !!
    !!     ! Locate peaks
    !!     call peak_detect(x, delta, imx, mx, imn, mn)
    !!
    !!     ! Get the t values at which the peaks/valleys were found
    !!     tmx = t(imx)
    !!     tmn = t(imn)
    !!
    !!     ! Plot
    !!     call plt%initialize()
    !!     xAxis => plt%get_x_axis()
    !!     yAxis => plt%get_y_axis()
    !!
    !!     call xAxis%set_title("t")
    !!     call yAxis%set_title("x(t)")
    !!
    !!     call d1%define_data(t, x)
    !!     call plt%push(d1)
    !!
    !!     call d2%define_data(tmx, mx)
    !!     call d2%set_draw_line(.false.)
    !!     call d2%set_draw_markers(.true.)
    !!     call d2%set_marker_style(MARKER_EMPTY_CIRCLE)
    !!     call d2%set_marker_scaling(1.5)
    !!     call d2%set_line_width(2.0)
    !!     call plt%push(d2)
    !!
    !!     call d3%define_data(tmn, mn)
    !!     call d3%set_draw_line(.false.)
    !!     call d3%set_draw_markers(.true.)
    !!     call d3%set_marker_style(MARKER_EMPTY_SQUARE)
    !!     call d3%set_marker_scaling(1.5)
    !!     call d3%set_line_width(2.0)
    !!     call plt%push(d3)
    !!
    !!     call plt%draw()
    !! end program
    !! @endcode
    !! @image html example_1.png
    interface peak_detect
        module procedure :: peak_detect_r32
        module procedure :: peak_detect_r64
    end interface
    
    ! peaks_implementation.f90
    interface
        module subroutine peak_detect_r32(x, delta, maxind, maxvals, minind, minvals)
            real(real32), intent(in) :: x(:)
            real(real32), intent(in) :: delta
            integer(int32), intent(out), allocatable :: maxind(:), minind(:)
            real(real32), intent(out), allocatable :: maxvals(:), minvals(:)
        end subroutine

        module subroutine peak_detect_r64(x, delta, maxind, maxvals, minind, minvals)
            real(real64), intent(in) :: x(:)
            real(real64), intent(in) :: delta
            integer(int32), intent(out), allocatable :: maxind(:), minind(:)
            real(real64), intent(out), allocatable :: maxvals(:), minvals(:)
        end subroutine
    end interface

end module