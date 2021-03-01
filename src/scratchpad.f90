subroutine downsize_mask(X, Y)
    use, intrinsic :: iso_c_binding
    implicit none

    logical(kind=c_bool), dimension(:, :), intent(in) :: X
    logical(kind=c_bool), dimension(:, :), intent(out) :: Y
    integer, dimension(2) :: src, dst
    integer :: src_width, src_height
    integer :: dst_width, dst_height
    integer :: i, j

    ! timing
    real :: t1, t2

    call cpu_time(t1)

    src = shape(X)
    src_width = src(1)
    src_height = src(2)

    dst = shape(Y)
    dst_width = dst(1)
    dst_height = dst(2)

    do concurrent(j=1:dst_height, i=1:dst_width)
        Y(i, j) = X(1 + (i - 1)*(src_width - 1)/(dst_width - 1), 1 + (j - 1)*(src_height - 1)/(dst_height - 1))
    end do

    call cpu_time(t2)

    print *, '[downsize_mask] SRC:', src, 'DST:', dst, 'elapsed:', 1000*(t2 - t1), '[ms]'

end subroutine downsize_mask

subroutine downsize_nn(X, Y)
    use, intrinsic :: iso_c_binding
    implicit none

    real(kind=c_float), dimension(:, :), intent(in) :: X
    real(kind=c_float), dimension(:, :), intent(out) :: Y
    integer, dimension(2) :: src, dst
    integer :: src_width, src_height
    integer :: dst_width, dst_height
    integer :: i, j

    ! timing
    real :: t1, t2

    call cpu_time(t1)

    src = shape(X)
    src_width = src(1)
    src_height = src(2)

    dst = shape(Y)
    dst_width = dst(1)
    dst_height = dst(2)

    do concurrent(j=1:dst_height, i=1:dst_width)
        Y(i, j) = X(1 + (i - 1)*(src_width - 1)/(dst_width - 1), 1 + (j - 1)*(src_height - 1)/(dst_height - 1))
    end do

    call cpu_time(t2)

    print *, '[downsize_nn] SRC:', src, 'DST:', dst, 'elapsed:', 1000*(t2 - t1), '[ms]'

end subroutine downsize_nn

subroutine downsize_linear(X, Y)
    use, intrinsic :: iso_c_binding
    implicit none

    ! the input array
    real(kind=c_float), dimension(:, :), intent(in) :: X

    ! the output (downsized) array
    real(kind=c_float), dimension(:, :), intent(out) :: Y

    ! a working array to handle image boundaries
    real(kind=c_float), dimension(:, :), allocatable :: W

    integer, dimension(2) :: src, dst
    integer :: src_width, src_height
    integer :: dst_width, dst_height

    ! source coordinates
    real :: Xs, Ys

    ! downsized destination coordinates
    integer :: Xd, Yd

    ! nearby-pixels
    real :: Xs0, Ys0, Xs1, Ys1

    ! interpolated values
    real :: I0, I1

    ! timing
    real :: t1, t2

    call cpu_time(t1)

    src = shape(X)
    src_width = src(1)
    src_height = src(2)

    dst = shape(Y)
    dst_width = dst(1)
    dst_height = dst(2)

    allocate (W(src_width + 1, src_height + 1))

    ! copy the image
    W(1:src_width, 1:src_height) = X

    ! then add the boundaries
    ! (Fourier transform origin, use reflection)
    W(src_width + 1, :) = X(1, :)
    W(:, src_height + 1) = X(:, 1)

    do concurrent(Yd=1:dst_height, Xd=1:dst_width)
        Xs = 1 + real(Xd - 1)*real(src_width - 1)/real(dst_width - 1)
        Ys = 1 + real(Yd - 1)*real(src_height - 1)/real(dst_height - 1)

        Xs0 = nint(Xs)
        Ys0 = nint(Ys)

        Xs1 = Xs0 + 1
        Ys1 = Ys0 + 1

        I0 = W(int(Xs0), int(Ys0))*(Xs1 - Xs) + W(int(Xs1), int(Ys0))*(Xs - Xs0)
        I1 = W(int(Xs0), int(Ys1))*(Xs1 - Xs) + W(int(Xs1), int(Ys1))*(Xs - Xs0)

        ! Linear Interpolation
        Y(Xd, Yd) = I0*(Ys1 - Ys) + I1*(Ys - Ys0)
    end do

    call cpu_time(t2)

    print *, '[downsize_linear] SRC:', src, 'DST:', dst, 'elapsed:', 1000*(t2 - t1), '[ms]'

end subroutine downsize_linear

elemental function Lanczos2(x)
    real :: Lanczos2
    real, intent(in) :: x
    real arg, sinc1, sinc2

    if (abs(x) .lt. 2) then
        arg = PI*x
        sinc1 = sin(arg)/arg
        sinc2 = sin(arg/2)/(arg/2)

        Lanczos2 = sinc1*sinc2
    else
        Lanczos2 = 0.0
    end if
end function Lanczos2

elemental function Lanczos3(x)
    real :: Lanczos3
    real, intent(in) :: x
    real arg, sinc1, sinc2

    if (abs(x) .lt. 3) then
        arg = PI*x
        sinc1 = sin(arg)/arg
        sinc2 = sin(arg/3)/(arg/3)

        Lanczos3 = sinc1*sinc2
    else
        Lanczos3 = 0.0
    end if
end function Lanczos3

subroutine downsize_lanczos_2(X, Y)
    use, intrinsic :: iso_c_binding
    implicit none

    ! the input array
    real(kind=c_float), dimension(:, :), intent(in) :: X

    ! the output (downsized) array
    real(kind=c_float), dimension(:, :), intent(out) :: Y

    ! a working array to handle image boundaries
    real(kind=c_float), dimension(:, :), allocatable :: W

    integer, dimension(2) :: src, dst
    integer :: src_width, src_height
    integer :: dst_width, dst_height

    ! source coordinates
    real :: Xs, Ys

    ! downsized destination coordinates
    integer :: Xd, Yd

    ! nearby-pixels
    real :: Xs0, Ys0, Xs1, Ys1, Xs2, Ys2, Xs3, Ys3

    ! interpolated values
    real :: I0, I1, I2, I3
    real :: a0, a1, a2, a3
    real :: b0, b1, b2, b3

    ! timing
    real :: t1, t2

    call cpu_time(t1)

    src = shape(X)
    src_width = src(1)
    src_height = src(2)

    dst = shape(Y)
    dst_width = dst(1)
    dst_height = dst(2)

    allocate (W(0:src_width + 2, 0:src_height + 2))

    ! copy the image
    W(1:src_width, 1:src_height) = X

    ! then add the boundaries
    ! (Fourier transform origin, use reflection)
    W(0, :) = X(src_width, :)
    W(src_width + 1, :) = X(1, :)
    W(src_width + 2, :) = X(2, :)

    W(:, 0) = X(:, src_height)
    W(:, src_height + 1) = X(:, 1)
    W(:, src_height + 2) = X(:, 2)

    ! the corners are not handled, we should be using the IPP

    do concurrent(Yd=1:dst_height, Xd=1:dst_width)

        Xs = 1 + real(Xd - 1)*real(src_width - 1)/real(dst_width - 1)
        Ys = 1 + real(Yd - 1)*real(src_height - 1)/real(dst_height - 1)

        Xs0 = nint(Xs) - 1
        Ys0 = nint(Ys) - 1

        Xs1 = Xs0 + 1
        Ys1 = Ys0 + 1

        Xs2 = Xs0 + 2
        Ys2 = Ys0 + 2

        Xs3 = Xs0 + 3
        Ys3 = Ys0 + 3

        ! Lanczos coefficients
        ! they should really be pre-calculated outside the loop
        a0 = Lanczos2(Xs - Xs0)
        a1 = Lanczos2(Xs - Xs1)
        a2 = Lanczos2(Xs - Xs2)
        a3 = Lanczos2(Xs - Xs3)

        b0 = Lanczos2(Ys - Ys0)
        b1 = Lanczos2(Ys - Ys1)
        b2 = Lanczos2(Ys - Ys2)
        b3 = Lanczos2(Ys - Ys3)

        ! intermediate intensities
        I0 = a0*W(int(Xs0), int(Ys0)) + &
             a1*W(int(Xs1), int(Ys0)) + &
             a2*W(int(Xs2), int(Ys0)) + &
             a3*W(int(Xs3), int(Ys0))

        I1 = a0*W(int(Xs0), int(Ys1)) + &
             a1*W(int(Xs1), int(Ys1)) + &
             a2*W(int(Xs2), int(Ys1)) + &
             a3*W(int(Xs3), int(Ys1))

        I2 = a0*W(int(Xs0), int(Ys2)) + &
             a1*W(int(Xs1), int(Ys2)) + &
             a2*W(int(Xs2), int(Ys2)) + &
             a3*W(int(Xs3), int(Ys2))

        I3 = a0*W(int(Xs0), int(Ys3)) + &
             a1*W(int(Xs1), int(Ys3)) + &
             a2*W(int(Xs2), int(Ys3)) + &
             a3*W(int(Xs3), int(Ys3))

        ! 2-lobed Lanczos
        Y(Xd, Yd) = b0*I0 + b1*I1 + b2*I2 + b3*I3

    end do

    call cpu_time(t2)

    print *, '[downsize_lanczos_2] SRC:', src, 'DST:', dst, 'elapsed:', 1000*(t2 - t1), '[ms]'

end subroutine downsize_lanczos_2

subroutine downsize_lanczos_3(X, Y)
    use, intrinsic :: iso_c_binding
    implicit none

    ! the input array
    real(kind=c_float), dimension(:, :), intent(in) :: X

    ! the output (downsized) array
    real(kind=c_float), dimension(:, :), intent(out) :: Y

    ! a working array to handle image boundaries
    real(kind=c_float), dimension(:, :), allocatable :: W

    integer, dimension(2) :: src, dst
    integer :: src_width, src_height
    integer :: dst_width, dst_height

    ! source coordinates
    real :: Xs, Ys

    ! downsized destination coordinates
    integer :: Xd, Yd

    ! nearby-pixels
    real :: Xs0, Ys0, Xs1, Ys1, Xs2, Ys2, Xs3, Ys3, Xs4, Ys4, Xs5, Ys5

    ! interpolated values
    real :: I0, I1, I2, I3, I4, I5
    real :: a0, a1, a2, a3, a4, a5
    real :: b0, b1, b2, b3, b4, b5

    ! timing
    real :: t1, t2

    call cpu_time(t1)

    src = shape(X)
    src_width = src(1)
    src_height = src(2)

    dst = shape(Y)
    dst_width = dst(1)
    dst_height = dst(2)

    allocate (W(-5:src_width + 6, -5:src_height + 6))

    ! copy the image
    W(1:src_width, 1:src_height) = X

    ! then replicate the borders
    W(-5, :) = X(1, :)
    W(-4, :) = X(1, :)
    W(-3, :) = X(1, :)
    W(-2, :) = X(1, :)
    W(-1, :) = X(1, :)
    W(0, :) = X(1, :)
    W(src_width + 1, :) = X(src_width, :)
    W(src_width + 2, :) = X(src_width, :)
    W(src_width + 3, :) = X(src_width, :)
    W(src_width + 4, :) = X(src_width, :)
    W(src_width + 5, :) = X(src_width, :)
    W(src_width + 6, :) = X(src_width, :)

    W(:, -5) = X(:, 1)
    W(:, -4) = X(:, 1)
    W(:, -3) = X(:, 1)
    W(:, -2) = X(:, 1)
    W(:, -1) = X(:, 1)
    W(:, 0) = X(:, 1)
    W(:, src_height + 1) = X(:, src_height)
    W(:, src_height + 2) = X(:, src_height)
    W(:, src_height + 3) = X(:, src_height)
    W(:, src_height + 4) = X(:, src_height)
    W(:, src_height + 5) = X(:, src_height)
    W(:, src_height + 6) = X(:, src_height)

    ! the corners are not handled, we should be using the IPP

    do concurrent(Yd=1:dst_height, Xd=1:dst_width)

        Xs = 1 + real(Xd - 1)*real(src_width - 1)/real(dst_width - 1)
        Ys = 1 + real(Yd - 1)*real(src_height - 1)/real(dst_height - 1)

        Xs0 = nint(Xs) - 2
        Ys0 = nint(Ys) - 2

        Xs1 = Xs0 + 1
        Ys1 = Ys0 + 1

        Xs2 = Xs0 + 2
        Ys2 = Ys0 + 2

        Xs3 = Xs0 + 3
        Ys3 = Ys0 + 3

        Xs4 = Xs0 + 4
        Ys4 = Ys0 + 4

        Xs5 = Xs0 + 5
        Ys5 = Ys0 + 5

        ! Lanczos coefficients
        ! they should really be pre-calculated outside the loop
        a0 = Lanczos2(Xs - Xs0)
        a1 = Lanczos2(Xs - Xs1)
        a2 = Lanczos2(Xs - Xs2)
        a3 = Lanczos2(Xs - Xs3)
        a4 = Lanczos2(Xs - Xs4)
        a5 = Lanczos2(Xs - Xs5)

        b0 = Lanczos2(Ys - Ys0)
        b1 = Lanczos2(Ys - Ys1)
        b2 = Lanczos2(Ys - Ys2)
        b3 = Lanczos2(Ys - Ys3)
        b4 = Lanczos2(Ys - Ys4)
        b5 = Lanczos2(Ys - Ys5)

        ! intermediate intensities
        I0 = a0*W(int(Xs0), int(Ys0)) + &
             a1*W(int(Xs1), int(Ys0)) + &
             a2*W(int(Xs2), int(Ys0)) + &
             a3*W(int(Xs3), int(Ys0)) + &
             a4*W(int(Xs4), int(Ys0)) + &
             a5*W(int(Xs5), int(Ys0))

        I1 = a0*W(int(Xs0), int(Ys1)) + &
             a1*W(int(Xs1), int(Ys1)) + &
             a2*W(int(Xs2), int(Ys1)) + &
             a3*W(int(Xs3), int(Ys1)) + &
             a4*W(int(Xs4), int(Ys1)) + &
             a5*W(int(Xs5), int(Ys1))

        I2 = a0*W(int(Xs0), int(Ys2)) + &
             a1*W(int(Xs1), int(Ys2)) + &
             a2*W(int(Xs2), int(Ys2)) + &
             a3*W(int(Xs3), int(Ys2)) + &
             a4*W(int(Xs4), int(Ys2)) + &
             a5*W(int(Xs5), int(Ys2))

        I3 = a0*W(int(Xs0), int(Ys3)) + &
             a1*W(int(Xs1), int(Ys3)) + &
             a2*W(int(Xs2), int(Ys3)) + &
             a3*W(int(Xs3), int(Ys3)) + &
             a4*W(int(Xs4), int(Ys3)) + &
             a5*W(int(Xs5), int(Ys3))

        I4 = a0*W(int(Xs0), int(Ys4)) + &
             a1*W(int(Xs1), int(Ys4)) + &
             a2*W(int(Xs2), int(Ys4)) + &
             a3*W(int(Xs3), int(Ys4)) + &
             a4*W(int(Xs4), int(Ys4)) + &
             a5*W(int(Xs5), int(Ys4))

        I5 = a0*W(int(Xs0), int(Ys5)) + &
             a1*W(int(Xs1), int(Ys5)) + &
             a2*W(int(Xs2), int(Ys5)) + &
             a3*W(int(Xs3), int(Ys5)) + &
             a4*W(int(Xs4), int(Ys5)) + &
             a5*W(int(Xs5), int(Ys5))

        ! 3-lobed Lanczos
        Y(Xd, Yd) = b0*I0 + b1*I1 + b2*I2 + b3*I3 + b4*I4 + b5*I5

    end do

    call cpu_time(t2)

    print *, '[downsize_lanczos_3] SRC:', src, 'DST:', dst, 'elapsed:', 1000*(t2 - t1), '[ms]'

end subroutine downsize_lanczos_3
