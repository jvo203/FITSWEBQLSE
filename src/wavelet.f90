module wavelet
    private i4_wrap, i4_modp, i4_reflect, i4_periodic

    ! the minimum X-Y dimension below which the wavelet routines should terminate
    ! the operation (i.e. 8 -> 4x4 coarse coefficients, ideal for ZFP)
    ! since ZFP operates on 4x4 blocks
    integer(kind=4), parameter :: min_dim = 8
contains
    subroutine daub4_transform(n, x, y)

!*****************************************************************************80
!
!! DAUB4_TRANSFORM computes the DAUB4 transform of a vector.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 July 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the dimension of the vector.
!    N must be a power of 2 and at least 4.
!
!    Input, real ( kind = 4 ) X(N), the vector to be transformed.
!
!    Output, real ( kind = 4 ) Y(N), the transformed vector.
!
        implicit none

        integer(kind=4) n
        integer(kind=4), parameter :: p = 3

        real(kind=4), dimension(0:p) :: c = (/ &
                                        0.4829629131445341E+00, &
                                        0.8365163037378079E+00, &
                                        0.2241438680420133E+00, &
                                        -0.1294095225512603E+00/)
        integer(kind=4) i
        integer(kind=4) j
        integer(kind=4) j0
        integer(kind=4) j1
        integer(kind=4) j2
        integer(kind=4) j3
        integer(kind=4) m
        real(kind=4) x(n)
        real(kind=4) y(n)
        real(kind=4) z(n)

        y(1:n) = x(1:n)
        z(1:n) = 0.0E+00

        m = n

        do while (4 <= m)

            i = 1

            do j = 1, m - 1, 2

                j0 = i4_periodic(j, 1, m)
                j1 = i4_periodic(j + 1, 1, m)
                j2 = i4_periodic(j + 2, 1, m)
                j3 = i4_periodic(j + 3, 1, m)

                z(i) = c(0)*y(j0) + c(1)*y(j1) &
                       + c(2)*y(j2) + c(3)*y(j3)

                z(i + m/2) = c(3)*y(j0) - c(2)*y(j1) &
                             + c(1)*y(j2) - c(0)*y(j3)

                i = i + 1

            end do

            y(1:m) = z(1:m)

            m = m/2

        end do

        return
    end subroutine daub4_transform

    subroutine daub4_transform_inverse(n, y, x)

!*****************************************************************************80
!
!! DAUB4_TRANSFORM_INVERSE inverts the DAUB4 transform of a vector.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 July 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the dimension of the vector.
!    N must be a power of 2 and at least 4.
!
!    Input, real ( kind = 4 ) Y(N), the transformed vector.
!
!    Output, real ( kind = 4 ) X(N), the original vector.
!
        implicit none

        integer(kind=4) n
        integer(kind=4), parameter :: p = 3

        real(kind=4), dimension(0:p) :: c = (/ &
                                        0.4829629131445341E+00, &
                                        0.8365163037378079E+00, &
                                        0.2241438680420133E+00, &
                                        -0.1294095225512603E+00/)
        integer(kind=4) i
        integer(kind=4) i0
        integer(kind=4) i1
        integer(kind=4) i2
        integer(kind=4) i3
        integer(kind=4) j
        integer(kind=4) m
        real(kind=4) x(n)
        real(kind=4) y(n)
        real(kind=4) z(n)

        x(1:n) = y(1:n)
        z(1:n) = 0.0E+00

        m = 4

        do while (m <= n)

            j = 1

            do i = 0, m/2 - 1

                i0 = i4_periodic(i, 1, m/2)
                i2 = i4_periodic(i + 1, 1, m/2)

                i1 = i4_periodic(i + m/2, m/2 + 1, m)
                i3 = i4_periodic(i + m/2 + 1, m/2 + 1, m)

                z(j) = c(2)*x(i0) + c(1)*x(i1) &
                       + c(0)*x(i2) + c(3)*x(i3)

                z(j + 1) = c(3)*x(i0) - c(0)*x(i1) &
                           + c(1)*x(i2) - c(2)*x(i3)

                j = j + 2

            end do

            x(1:m) = z(1:m)

            m = m*2

        end do

        return
    end subroutine daub4_transform_inverse

    function i4_modp(i, j)

        !*****************************************************************************80
        !
    !! I4_MODP returns the nonnegative remainder of I4 division.
        !
        !  Discussion:
        !
        !    If
        !      NREM = I4_MODP ( I, J )
        !      NMULT = ( I - NREM ) / J
        !    then
        !      I = J * NMULT + NREM
        !    where NREM is always nonnegative.
        !
        !    The MOD function computes a result with the same sign as the
        !    quantity being divided.  Thus, suppose you had an angle A,
        !    and you wanted to ensure that it was between 0 and 360.
        !    Then mod(A,360) would do, if A was positive, but if A
        !    was negative, your result would be between -360 and 0.
        !
        !    On the other hand, I4_MODP(A,360) is between 0 and 360, always.
        !
        !    An I4 is an integer ( kind = 4 ) value.
        !
        !  Example:
        !
        !        I     J     MOD I4_MODP    Factorization
        !
        !      107    50       7       7    107 =  2 *  50 + 7
        !      107   -50       7       7    107 = -2 * -50 + 7
        !     -107    50      -7      43   -107 = -3 *  50 + 43
        !     -107   -50      -7      43   -107 =  3 * -50 + 43
        !
        !  Licensing:
        !
        !    This code is distributed under the GNU LGPL license.
        !
        !  Modified:
        !
        !    02 March 1999
        !
        !  Author:
        !
        !    John Burkardt
        !
        !  Parameters:
        !
        !    Input, integer ( kind = 4 ) I, the number to be divided.
        !
        !    Input, integer ( kind = 4 ) J, the number that divides I.
        !
        !    Output, integer ( kind = 4 ) I4_MODP, the nonnegative remainder when I is
        !    divided by J.
        !
        implicit none

        integer(kind=4), intent(in) :: i
        integer(kind=4) i4_modp
        integer(kind=4), intent(in) :: j
        integer(kind=4) value

        if (j == 0) then
            write (*, '(a)') ' '
            write (*, '(a)') 'I4_MODP - Fatal error!'
            write (*, '(a,i8)') '  Illegal divisor J = ', j
            stop
        end if

        value = mod(i, j)

        if (value < 0) then
            value = value + abs(j)
        end if

        i4_modp = value

        return
    end function i4_modp

    function i4_wrap(ival, ilo, ihi)

        !*****************************************************************************80
        !
    !! I4_WRAP forces an I4 to lie between given limits by wrapping.
        !
        !  Discussion:
        !
        !    An I4 is an integer ( kind = 4 ) value.
        !
        !    There appears to be a bug in the GFORTRAN compiler which can lead to
        !    erroneous results when the first argument of I4_WRAP is an expression.
        !    In particular:
        !
        !    do i = 1, 3
        !      if ( test ) then
        !        i4 = i4_wrap ( i + 1, 1, 3 )
        !      end if
        !    end do
        !
        !    was, when I = 3, returning I4 = 3.  So I had to replace this with
        !
        !    do i = 1, 3
        !      if ( test ) then
        !        i4 = i + 1
        !        i4 = i4_wrap ( i4, 1, 3 )
        !      end if
        !    end do
        !
        !  Example:
        !
        !    ILO = 4, IHI = 8
        !
        !    I  Value
        !
        !    -2     8
        !    -1     4
        !     0     5
        !     1     6
        !     2     7
        !     3     8
        !     4     4
        !     5     5
        !     6     6
        !     7     7
        !     8     8
        !     9     4
        !    10     5
        !    11     6
        !    12     7
        !    13     8
        !    14     4
        !
        !  Licensing:
        !
        !    This code is distributed under the GNU LGPL license.
        !
        !  Modified:
        !
        !    07 September 2009
        !
        !  Author:
        !
        !    John Burkardt
        !
        !  Parameters:
        !
        !    Input, integer ( kind = 4 ) IVAL, a value.
        !
        !    Input, integer ( kind = 4 ) ILO, IHI, the desired bounds.
        !
        !    Output, integer ( kind = 4 ) I4_WRAP, a "wrapped" version of the value.
        !
        implicit none

        integer(kind=4) i4_wrap
        integer(kind=4), intent(in) :: ihi
        integer(kind=4), intent(in) :: ilo
        integer(kind=4), intent(in) :: ival
        integer(kind=4) jhi
        integer(kind=4) jlo
        integer(kind=4) value
        integer(kind=4) wide

        jlo = min(ilo, ihi)
        jhi = max(ilo, ihi)

        wide = jhi - jlo + 1

        if (wide == 1) then
            value = jlo
        else
            value = jlo + i4_modp(ival - jlo, wide)
        end if

        i4_wrap = value

        return
    end function i4_wrap

    elemental function i4_reflect(ival, ilo, ihi)
        integer(kind=4) :: i4_reflect
        integer(kind=4), intent(in) :: ival, ilo, ihi
        integer(kind=4) :: value
        integer(kind=4) :: dist

        ! the default return value
        value = ival

        ! the upper boundary
        if (ival .gt. ihi) then
            dist = ival - ihi
            value = ihi - (dist - 1)
        end if

        ! the lower boundary
        if (ival .lt. ilo) then
            dist = ilo - ival
            value = ilo + (dist - 1)
        end if

        i4_reflect = value

        return
    end function i4_reflect

    elemental function i4_periodic(ival, ilo, ihi)
        integer(kind=4) :: i4_periodic
        integer(kind=4), intent(in) :: ival, ilo, ihi
        integer(kind=4) :: value
        integer(kind=4) :: dist

        ! the default return value
        value = ival

        ! the upper boundary
        if (ival .gt. ihi) then
            dist = ival - ihi
            value = ilo + (dist - 1)
        end if

        ! the lower boundary
        if (ival .lt. ilo) then
            dist = ilo - ival
            value = ihi - (dist - 1)
        end if

        i4_periodic = value

        return
    end function i4_periodic

    subroutine daub4_2Dtransform(n, x, y, mask, create_mask)
!********************************
! a 2D DAUB4 wavelet transform of a square floating-point matrix
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Author:
!
!  Christopher Zapart, based on the 1D code by John Burkardt
!
!  Input, integer (kind = 4) N, the dimension of the square block N x N
!    N must be a power of 2 and at least 4.
!
!  Input, real (kind = 4) X(N,N), the original square matrix to be transformed
!
!  Output, real (kind = 4) Y(N,N), the wavelet-transformed matrix
!
! Output, logical mask(N,N), a boolean matrix where .false. indicates NaN values in X
!
! Input: logical create_mask - whether or not to creater or re-use the given mask
! instead of creating it from scratch
!
        implicit none

        integer(kind=4) :: n
        integer(kind=4), parameter :: p = 3
        real(kind=4), parameter :: zero = 1.0E-05

        real(kind=4), dimension(0:p) :: c = (/ &
                                        0.4829629131445341E+00, &
                                        0.8365163037378079E+00, &
                                        0.2241438680420133E+00, &
                                        -0.1294095225512603E+00/)
        integer(kind=4) i
        integer(kind=4) j
        integer(kind=4) j0
        integer(kind=4) j1
        integer(kind=4) j2
        integer(kind=4) j3

        integer(kind=4) m
        real(kind=4), dimension(n, n), intent(in) :: x
        real(kind=4), dimension(n, n), intent(out) :: y
        logical(kind=1), optional, dimension(n, n), intent(inout) :: mask
        logical, optional, intent(in) :: create_mask
        real(kind=4), dimension(n) :: z
        real(kind=4) average
        integer(kind=4) nvalid
        integer(kind=4) k

        y = x
        z = 0.0E+00

        if (present(mask)) then
            block
                logical need_mask

                if (present(create_mask)) then
                    need_mask = create_mask
                else
                    need_mask = .true.
                end if

                if (need_mask) then
                    !  pick out all the NaN
                    where (isnan(x))
                        mask = .false.
                    elsewhere
                        mask = .true.
                    end where
                end if

                ! calculate the mean of the input array
                nvalid = count(mask)

                ! all values might be NaN in which case set the array to 0.0
                if (nvalid .gt. 0) then
                    average = sum(x, mask=mask)/nvalid
                else
                    average = 0.0
                end if

                ! replace NaNs with the average value
                where (.not. mask) y = average
            end block
        end if

        m = n

        ! for each level
        do while (m .ge. min_dim)
            ! transform all the rows
            do k = 1, m
                i = 1

                do j = 1, m - 1, 2

                    j0 = i4_periodic(j, 1, m)
                    j1 = i4_periodic(j + 1, 1, m)
                    j2 = i4_periodic(j + 2, 1, m)
                    j3 = i4_periodic(j + 3, 1, m)

                    z(i) = c(0)*y(k, j0) + c(1)*y(k, j1) &
                           + c(2)*y(k, j2) + c(3)*y(k, j3)

                    z(i + m/2) = c(3)*y(k, j0) - c(2)*y(k, j1) &
                                 + c(1)*y(k, j2) - c(0)*y(k, j3)

                    i = i + 1

                end do

                y(k, 1:m) = z(1:m)
            end do

            ! then the columns
            do k = 1, m
                i = 1

                do j = 1, m - 1, 2

                    j0 = i4_periodic(j, 1, m)
                    j1 = i4_periodic(j + 1, 1, m)
                    j2 = i4_periodic(j + 2, 1, m)
                    j3 = i4_periodic(j + 3, 1, m)

                    z(i) = c(0)*y(j0, k) + c(1)*y(j1, k) &
                           + c(2)*y(j2, k) + c(3)*y(j3, k)

                    z(i + m/2) = c(3)*y(j0, k) - c(2)*y(j1, k) &
                                 + c(1)*y(j2, k) - c(0)*y(j3, k)

                    i = i + 1

                end do

                y(1:m, k) = z(1:m)
            end do

            m = m/2
        end do

        ! truncate values near zero
        where (abs(y) < zero) y = 0.0

        return
    end subroutine daub4_2Dtransform

    subroutine daub4_2Dtransform_inv(n, y, x, mask)
        !********************************
        ! a 2D DAUB4 inverse wavelet transform of a square floating-point matrix
        !
        !  Licensing:
        !
        !    This code is distributed under the GNU LGPL license.
        !
        !  Author:
        !
        !  Christopher Zapart, based on the 1D code by John Burkardt
        !
        !  Input, integer (kind = 4) N, the dimension of the square block N x N
        !    N must be a power of 2 and at least 4.
        !
        !  Input, real (kind = 4) Y(N,N), the wavelet-transformed square matrix
        !
        !  Output, real (kind = 4) X(N,N), the original matrix
        !
        use, intrinsic :: ieee_arithmetic
        implicit none

        integer(kind=4) n
        integer(kind=4), parameter :: p = 3

        real(kind=4), dimension(0:p) :: c = (/ &
                                        0.4829629131445341E+00, &
                                        0.8365163037378079E+00, &
                                        0.2241438680420133E+00, &
                                        -0.1294095225512603E+00/)
        integer(kind=4) i
        integer(kind=4) i0
        integer(kind=4) i1
        integer(kind=4) i2
        integer(kind=4) i3
        integer(kind=4) j
        integer(kind=4) m
        logical(kind=1), optional, dimension(n, n), intent(in) :: mask
        real(kind=4), dimension(n, n), intent(out) :: x
        real(kind=4), dimension(n, n), intent(in) :: y
        real(kind=4), dimension(n) :: z

        integer(kind=4) k

        x = y
        z = 0.0E+00

        m = min_dim

        ! for each level
        do while (m .le. n)
            ! reverse all the columns
            do k = 1, m
                j = 1

                do i = 0, m/2 - 1

                    i0 = i4_periodic(i, 1, m/2)
                    i2 = i4_periodic(i + 1, 1, m/2)

                    i1 = i4_periodic(i + m/2, m/2 + 1, m)
                    i3 = i4_periodic(i + m/2 + 1, m/2 + 1, m)

                    z(j) = c(2)*x(i0, k) + c(1)*x(i1, k) &
                           + c(0)*x(i2, k) + c(3)*x(i3, k)

                    z(j + 1) = c(3)*x(i0, k) - c(0)*x(i1, k) &
                               + c(1)*x(i2, k) - c(2)*x(i3, k)

                    j = j + 2

                end do

                x(1:m, k) = z(1:m)
            end do

            ! then the rows
            do k = 1, m
                j = 1

                do i = 0, m/2 - 1

                    i0 = i4_periodic(i, 1, m/2)
                    i2 = i4_periodic(i + 1, 1, m/2)

                    i1 = i4_periodic(i + m/2, m/2 + 1, m)
                    i3 = i4_periodic(i + m/2 + 1, m/2 + 1, m)

                    z(j) = c(2)*x(k, i0) + c(1)*x(k, i1) &
                           + c(0)*x(k, i2) + c(3)*x(k, i3)

                    z(j + 1) = c(3)*x(k, i0) - c(0)*x(k, i1) &
                               + c(1)*x(k, i2) - c(2)*x(k, i3)

                    j = j + 2

                end do

                x(k, 1:m) = z(1:m)
            end do

            m = m*2
        end do

        ! insert back NaN values
        if (present(mask)) then
            where (.not. mask) x = ieee_value(0.0, ieee_quiet_nan)
        end if

        return
    end subroutine daub4_2Dtransform_inv

! in-place versions
    subroutine daub4_2Dtransform_inpl(n, x, mask, create_mask)
        !********************************
        ! a 2D DAUB4 wavelet transform of a square floating-point matrix (in-place)
        !
        !  Licensing:
        !
        !    This code is distributed under the GNU LGPL license.
        !
        !  Author:
        !
        !  Christopher Zapart, based on the 1D code by John Burkardt
        !
        !  Input, integer (kind = 4) N, the dimension of the square block N x N
        !    N must be a power of 2 and at least 4.
        !
        !  Input and Output, real (kind = 4) X(N,N), the original square matrix to be transformed
        !
        ! Output, logical mask(N,N), a boolean matrix where .false. indicates NaN values in X
        !
        !  the output is stored in-place
        !
        ! Input: logical create_mask - whether or not to creater or re-use the given mask
        ! instead of creating it from scratch
        !
        implicit none

        integer(kind=4) :: n
        integer(kind=4), parameter :: p = 3
        real(kind=4), parameter :: zero = 1.0E-05

        real(kind=4), dimension(0:p) :: c = (/ &
                                        0.4829629131445341E+00, &
                                        0.8365163037378079E+00, &
                                        0.2241438680420133E+00, &
                                        -0.1294095225512603E+00/)
        integer(kind=4) i
        integer(kind=4) j
        integer(kind=4) j0
        integer(kind=4) j1
        integer(kind=4) j2
        integer(kind=4) j3

        integer(kind=4) m
        real(kind=4), dimension(n, n), intent(inout) :: x
        logical(kind=1), optional, dimension(n, n), intent(inout) :: mask
        logical, optional, intent(in) :: create_mask
        real(kind=4), dimension(n) :: z
        real(kind=4) average
        integer(kind=4) nvalid
        integer(kind=4) k

        z = 0.0E+00

        if (present(mask)) then
            block
                logical need_mask

                if (present(create_mask)) then
                    need_mask = create_mask
                else
                    need_mask = .true.
                end if

                if (need_mask) then
                    !  pick out all the NaN
                    where (isnan(x))
                        mask = .false.
                    elsewhere
                        mask = .true.
                    end where
                end if

                ! calculate the mean of the input array
                nvalid = count(mask)

                ! all values might be NaN in which case set the array to 0.0
                if (nvalid .gt. 0) then
                    average = sum(x, mask=mask)/nvalid
                else
                    average = 0.0
                end if

                ! replace NaNs with the average value
                where (.not. mask) x = average
            end block
        end if

        m = n

        ! for each level
        do while (m .ge. min_dim)
            ! transform all the rows
            do k = 1, m
                i = 1

                do j = 1, m - 1, 2

                    j0 = i4_periodic(j, 1, m)
                    j1 = i4_periodic(j + 1, 1, m)
                    j2 = i4_periodic(j + 2, 1, m)
                    j3 = i4_periodic(j + 3, 1, m)

                    z(i) = c(0)*x(k, j0) + c(1)*x(k, j1) &
                           + c(2)*x(k, j2) + c(3)*x(k, j3)

                    z(i + m/2) = c(3)*x(k, j0) - c(2)*x(k, j1) &
                                 + c(1)*x(k, j2) - c(0)*x(k, j3)

                    i = i + 1

                end do

                x(k, 1:m) = z(1:m)
            end do

            ! then the columns
            do k = 1, m
                i = 1

                do j = 1, m - 1, 2

                    j0 = i4_periodic(j, 1, m)
                    j1 = i4_periodic(j + 1, 1, m)
                    j2 = i4_periodic(j + 2, 1, m)
                    j3 = i4_periodic(j + 3, 1, m)

                    z(i) = c(0)*x(j0, k) + c(1)*x(j1, k) &
                           + c(2)*x(j2, k) + c(3)*x(j3, k)

                    z(i + m/2) = c(3)*x(j0, k) - c(2)*x(j1, k) &
                                 + c(1)*x(j2, k) - c(0)*x(j3, k)

                    i = i + 1

                end do

                x(1:m, k) = z(1:m)
            end do

            m = m/2
        end do

        ! truncate values near zero
        where (abs(x) .lt. zero) x = 0.0

        return
    end subroutine daub4_2Dtransform_inpl

    subroutine daub4_2Dtransform_inv_inpl(n, x, mask)
        !********************************
        ! a 2D DAUB4 inverse wavelet transform of a square floating-point matrix (in-place)
        !
        !  Licensing:
        !
        !    This code is distributed under the GNU LGPL license.
        !
        !  Author:
        !
        !  Christopher Zapart, based on the 1D code by John Burkardt
        !
        !  Input, integer (kind = 4) N, the dimension of the square block N x N
        !    N must be a power of 2 and at least 4.
        !
        !  Input and Output, real (kind = 4) X(N,N), the wavelet-transformed square matrix
        !
        !  the output is stored in-place in x
        !
        use, intrinsic :: ieee_arithmetic
        implicit none

        integer(kind=4) n
        integer(kind=4), parameter :: p = 3

        real(kind=4), dimension(0:p) :: c = (/ &
                                        0.4829629131445341E+00, &
                                        0.8365163037378079E+00, &
                                        0.2241438680420133E+00, &
                                        -0.1294095225512603E+00/)
        integer(kind=4) i
        integer(kind=4) i0
        integer(kind=4) i1
        integer(kind=4) i2
        integer(kind=4) i3
        integer(kind=4) j
        integer(kind=4) m
        real(kind=4), dimension(n, n), intent(inout) :: x
        logical(kind=1), optional, dimension(n, n), intent(in) :: mask
        real(kind=4), dimension(n) :: z

        integer(kind=4) k

        z = 0.0E+00

        m = min_dim

        ! for each level
        do while (m .le. n)
            ! reverse all the columns
            do k = 1, m
                j = 1

                do i = 0, m/2 - 1

                    i0 = i4_periodic(i, 1, m/2)
                    i2 = i4_periodic(i + 1, 1, m/2)

                    i1 = i4_periodic(i + m/2, m/2 + 1, m)
                    i3 = i4_periodic(i + m/2 + 1, m/2 + 1, m)

                    z(j) = c(2)*x(i0, k) + c(1)*x(i1, k) &
                           + c(0)*x(i2, k) + c(3)*x(i3, k)

                    z(j + 1) = c(3)*x(i0, k) - c(0)*x(i1, k) &
                               + c(1)*x(i2, k) - c(2)*x(i3, k)

                    j = j + 2

                end do

                x(1:m, k) = z(1:m)
            end do

            ! then the rows
            do k = 1, m
                j = 1

                do i = 0, m/2 - 1

                    i0 = i4_periodic(i, 1, m/2)
                    i2 = i4_periodic(i + 1, 1, m/2)

                    i1 = i4_periodic(i + m/2, m/2 + 1, m)
                    i3 = i4_periodic(i + m/2 + 1, m/2 + 1, m)

                    z(j) = c(2)*x(k, i0) + c(1)*x(k, i1) &
                           + c(0)*x(k, i2) + c(3)*x(k, i3)

                    z(j + 1) = c(3)*x(k, i0) - c(0)*x(k, i1) &
                               + c(1)*x(k, i2) - c(2)*x(k, i3)

                    j = j + 2

                end do

                x(k, 1:m) = z(1:m)
            end do

            m = m*2
        end do

        ! insert back NaN values
        if (present(mask)) then
            where (.not. mask) x = ieee_value(0.0, ieee_quiet_nan)
        end if

        return
    end subroutine daub4_2Dtransform_inv_inpl

    subroutine wave_shrink(n, x)
        implicit none

        integer(kind=4) :: n
        real(kind=4), dimension(n, n), intent(inout) :: x
        integer(kind=4) :: i, j, nvalid
        real(kind=4) mean, std, tmp

        if (n .lt. min_dim) return

        ! calculate the mean and standard deviation of
        ! absolute values of detail coefficients (skipping the coarse coeffs)

        mean = 0.0
        std = 0.0
        nvalid = 0

        ! the mean
        do j = 1, n
            do i = 1, n
                ! skip the coarse coefficients
                if ((i .le. min_dim/2) .and. (j .le. min_dim/2)) cycle

                tmp = abs(x(i, j))
                ! only process non-zero values
                if (tmp .gt. 0.0) then
                    mean = mean + tmp
                    nvalid = nvalid + 1
                end if
            end do
        end do

        if (nvalid .gt. 0) then
            mean = mean/nvalid
        else
            return
        end if

        ! do nothing if all values are 0.0
        if (mean .eq. 0.0) return

        ! the standard deviation
        do j = 1, n
            do i = 1, n
                ! skip the coarse coefficients
                if ((i .le. 2) .and. (j .le. 2)) cycle

                tmp = abs(x(i, j))
                ! only process non-zero values
                if (tmp .gt. 0.0) std = std + (tmp - mean)**2
            end do
        end do

        if ((nvalid - 1) .gt. 0) then
            std = sqrt(std/(nvalid - 1))
        else if (nvalid .gt. 0) then
            ! nvalid .eq. 1 at this point (there is no point in dividing by 1)
            std = sqrt(std/nvalid)
        else
            return
        end if

        print *, '[wave_shrink] mean:', mean, 'std:', std

        ! prune (shrink the coefficients)
        do j = 1, n
            do i = 1, n
                ! skip the coarse coefficients
                if ((i .le. 2) .and. (j .le. 2)) cycle

                ! set the small coefficients (one half to be precise) to 0.0
                if (abs(x(i, j)) .le. mean - 0.5*std) x(i, j) = 0.0
            end do
        end do

    end subroutine wave_shrink

    subroutine wave_shrink2(n, x)
        implicit none

        integer(kind=4) :: n
        real(kind=4), dimension(n, n), intent(inout) :: x
        integer(kind=4) :: i, j, nvalid
        real(kind=4) mean, std, tmp

        if (n .lt. min_dim) return

        ! calculate the mean and standard deviation of
        ! of detail coefficients (skipping the coarse coeffs)

        mean = 0.0
        std = 0.0

        ! the mean
        do j = 1, n
            do i = 1, n
                ! skip the coarse coefficients
                if ((i .le. min_dim/2) .and. (j .le. min_dim/2)) cycle

                mean = mean + x(i, j)
                nvalid = nvalid + 1
            end do
        end do

        if (nvalid .gt. 0) then
            mean = mean/nvalid
        else
            return
        end if

        ! do nothing if all values are 0.0
        if (mean .eq. 0.0) return

        ! the standard deviation
        do j = 1, n
            do i = 1, n
                ! skip the coarse coefficients
                if ((i .le. 2) .and. (j .le. 2)) cycle

                tmp = x(i, j)
                std = std + (tmp - mean)*(tmp - mean)
            end do
        end do

        if ((nvalid - 1) .gt. 0) then
            std = sqrt(std/(nvalid - 1))
        else if (nvalid .gt. 0) then
            ! I know nvalid .eq. 1 at this point (there is no point dividing by 1)
            std = sqrt(std/nvalid)
        else
            return
        end if

        print *, 'mean:', mean, 'std:', std

        ! prune (shrink the coefficients)
        do j = 1, n
            do i = 1, n
                ! skip the coarse coefficients
                if ((i .le. 2) .and. (j .le. 2)) cycle

                ! set the small coefficients to 0.0
                tmp = x(i, j)
                if ((tmp .gt. (mean - 0.25*std)) .and. (tmp .lt. (mean + 0.25*std))) x(i, j) = 0.0
            end do
        end do

    end subroutine wave_shrink2

    subroutine to_fixed(n, x)
        implicit none

        integer(kind=4) :: n
        real(kind=4), dimension(n, n), intent(in) :: x
        integer(kind=4) :: i, j
        real(kind=4), dimension(4, 4) :: tmp

        if (mod(n, 4) .ne. 0) return
    end subroutine to_fixed
end module wavelet
