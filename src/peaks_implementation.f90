! peaks_implementation.f90
submodule (peaks) peaks_implementation
contains
! ------------------------------------------------------------------------------
module subroutine peak_detect_r32(x, delta, maxind, maxvals, minind, minvals)
    ! Arguments
    real(real32), intent(in) :: x(:)
    real(real32), intent(in) :: delta
    integer(int32), intent(out), allocatable :: maxind(:), minind(:)
    real(real32), intent(out), allocatable :: maxvals(:), minvals(:)

    ! Local Variables
    logical :: lookformax
    integer(int32) :: i, n, nmax, nmin, mxpos, mnpos
    integer(int32), allocatable :: maxibuffer(:), minibuffer(:)
    real(real32), allocatable :: maxbuffer(:), minbuffer(:)
    real(real32) :: this, mn, mx

    ! Initialization
    n = size(x)
    nmax = 0
    nmin = 0
    lookformax = .true.
    mn = huge(delta)
    mx = -huge(delta)
    allocate(maxibuffer(n))
    allocate(maxbuffer(n))
    allocate(minibuffer(n))
    allocate(minbuffer(n))

    ! Process
    do i = 1, n
        this = x(i)
        if (this > mx) then
            mx = this
            mxpos = i
        end if
        if (this < mn) then
            mn = this
            mnpos = i
        end if

        if (lookformax) then
            if (this < mx - delta) then
                nmax = nmax + 1
                maxibuffer(nmax) = mxpos
                maxbuffer(nmax) = mx
                mn = this
                mnpos = i
                lookformax = .false.
            end if
        else
            if (this > mn + delta) then
                nmin = nmin + 1
                minibuffer(nmin) = mnpos
                minbuffer(nmin) = mn
                mx = this
                mxpos = i
                lookformax = .true.
            end if
        end if
    end do

    ! Handle the output
    allocate(maxind(nmax), source = maxibuffer(1:nmax))
    allocate(maxvals(nmax), source = maxbuffer(1:nmax))
    allocate(minind(nmin), source = minibuffer(1:nmin))
    allocate(minvals(nmin), source = minbuffer(1:nmin))
end subroutine

! ------------------------------------------------------------------------------
module subroutine peak_detect_r64(x, delta, maxind, maxvals, minind, minvals)
    ! Arguments
    real(real64), intent(in) :: x(:)
    real(real64), intent(in) :: delta
    integer(int32), intent(out), allocatable :: maxind(:), minind(:)
    real(real64), intent(out), allocatable :: maxvals(:), minvals(:)

    ! Local Variables
    logical :: lookformax
    integer(int32) :: i, n, nmax, nmin, mxpos, mnpos
    integer(int32), allocatable :: maxibuffer(:), minibuffer(:)
    real(real64), allocatable :: maxbuffer(:), minbuffer(:)
    real(real64) :: this, mn, mx

    ! Initialization
    n = size(x)
    nmax = 0
    nmin = 0
    lookformax = .true.
    mn = huge(delta)
    mx = -huge(delta)
    allocate(maxibuffer(n))
    allocate(maxbuffer(n))
    allocate(minibuffer(n))
    allocate(minbuffer(n))

    ! Process
    do i = 1, n
        this = x(i)
        if (this > mx) then
            mx = this
            mxpos = i
        end if
        if (this < mn) then
            mn = this
            mnpos = i
        end if

        if (lookformax) then
            if (this < mx - delta) then
                nmax = nmax + 1
                maxibuffer(nmax) = mxpos
                maxbuffer(nmax) = mx
                mn = this
                mnpos = i
                lookformax = .false.
            end if
        else
            if (this > mn + delta) then
                nmin = nmin + 1
                minibuffer(nmin) = mnpos
                minbuffer(nmin) = mn
                mx = this
                mxpos = i
                lookformax = .true.
            end if
        end if
    end do

    ! Handle the output
    allocate(maxind(nmax), source = maxibuffer(1:nmax))
    allocate(maxvals(nmax), source = maxbuffer(1:nmax))
    allocate(minind(nmin), source = minibuffer(1:nmin))
    allocate(minvals(nmin), source = minbuffer(1:nmin))
end subroutine

! ------------------------------------------------------------------------------
end submodule