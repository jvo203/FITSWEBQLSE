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
    integer(kind=4) i4_wrap
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

            j0 = i4_wrap(j, 1, m)
            j1 = i4_wrap(j + 1, 1, m)
            j2 = i4_wrap(j + 2, 1, m)
            j3 = i4_wrap(j + 3, 1, m)

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
end
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
    integer(kind=4) i4_wrap
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

            i0 = i4_wrap(i, 1, m/2)
            i2 = i4_wrap(i + 1, 1, m/2)

            i1 = i4_wrap(i + m/2, m/2 + 1, m)
            i3 = i4_wrap(i + m/2 + 1, m/2 + 1, m)

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
end
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

    integer(kind=4) i
    integer(kind=4) i4_modp
    integer(kind=4) j
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
end
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

    integer(kind=4) i4_modp
    integer(kind=4) i4_wrap
    integer(kind=4) ihi
    integer(kind=4) ilo
    integer(kind=4) ival
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
end
subroutine daub4_2Dtransform(n, x, y)
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
    implicit none

    integer(kind=4) :: n
    integer(kind=4), parameter :: p = 3

    real(kind=4), dimension(0:p) :: c = (/ &
                                    0.4829629131445341E+00, &
                                    0.8365163037378079E+00, &
                                    0.2241438680420133E+00, &
                                    -0.1294095225512603E+00/)
    integer(kind=4) i
    integer(kind=4) i4_wrap
    integer(kind=4) j
    integer(kind=4) j0
    integer(kind=4) j1
    integer(kind=4) j2
    integer(kind=4) j3

    integer(kind=4) m
    real(kind=4), dimension(n, n), intent(in) :: x
    real(kind=4), dimension(n, n), intent(out) :: y
    real(kind=4), dimension(n) :: z

    integer(kind=4) k

    y = x
    z = 0.0E+00

    m = n

    ! for each level
    do while (m .ge. 4)
        ! transform all the rows
        do k = 1, m
            i = 1

            do j = 1, m - 1, 2

                j0 = i4_wrap(j, 1, m)
                j1 = i4_wrap(j + 1, 1, m)
                j2 = i4_wrap(j + 2, 1, m)
                j3 = i4_wrap(j + 3, 1, m)

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

                j0 = i4_wrap(j, 1, m)
                j1 = i4_wrap(j + 1, 1, m)
                j2 = i4_wrap(j + 2, 1, m)
                j3 = i4_wrap(j + 3, 1, m)

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

    return
end subroutine
subroutine daub4_2Dtransform_inv(n, y, x)
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
    integer(kind=4) i4_wrap
    integer(kind=4) j
    integer(kind=4) m
    real(kind=4), dimension(n, n), intent(out) :: x
    real(kind=4), dimension(n, n), intent(in) :: y
    real(kind=4), dimension(n) :: z

    integer(kind=4) k

    x = y
    z = 0.0E+00

    m = 4

    ! for each level
    do while (m .le. n)
        ! reverse all the columns
        do k = 1, m
            j = 1

            do i = 0, m/2 - 1

                i0 = i4_wrap(i, 1, m/2)
                i2 = i4_wrap(i + 1, 1, m/2)

                i1 = i4_wrap(i + m/2, m/2 + 1, m)
                i3 = i4_wrap(i + m/2 + 1, m/2 + 1, m)

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

                i0 = i4_wrap(i, 1, m/2)
                i2 = i4_wrap(i + 1, 1, m/2)

                i1 = i4_wrap(i + m/2, m/2 + 1, m)
                i3 = i4_wrap(i + m/2 + 1, m/2 + 1, m)

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

    return
end subroutine

! in-place versions
subroutine daub4_2Dtransform_inpl(n, x)
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
    !  the output is stored in-place
    !
    implicit none

    integer(kind=4) :: n
    integer(kind=4), parameter :: p = 3

    real(kind=4), dimension(0:p) :: c = (/ &
                                    0.4829629131445341E+00, &
                                    0.8365163037378079E+00, &
                                    0.2241438680420133E+00, &
                                    -0.1294095225512603E+00/)
    integer(kind=4) i
    integer(kind=4) i4_wrap
    integer(kind=4) j
    integer(kind=4) j0
    integer(kind=4) j1
    integer(kind=4) j2
    integer(kind=4) j3

    integer(kind=4) m
    real(kind=4), dimension(n, n), intent(inout) :: x
    real(kind=4), dimension(n) :: z

    integer(kind=4) k

    z = 0.0E+00

    m = n

    ! for each level
    do while (m .ge. 4)
        ! transform all the rows
        do k = 1, m
            i = 1

            do j = 1, m - 1, 2

                j0 = i4_wrap(j, 1, m)
                j1 = i4_wrap(j + 1, 1, m)
                j2 = i4_wrap(j + 2, 1, m)
                j3 = i4_wrap(j + 3, 1, m)

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

                j0 = i4_wrap(j, 1, m)
                j1 = i4_wrap(j + 1, 1, m)
                j2 = i4_wrap(j + 2, 1, m)
                j3 = i4_wrap(j + 3, 1, m)

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

    return
end subroutine
subroutine daub4_2Dtransform_inv_inpl(n, x)
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
    integer(kind=4) i4_wrap
    integer(kind=4) j
    integer(kind=4) m
    real(kind=4), dimension(n, n), intent(inout) :: x
    real(kind=4), dimension(n) :: z

    integer(kind=4) k

    z = 0.0E+00

    m = 4

    ! for each level
    do while (m .le. n)
        ! reverse all the columns
        do k = 1, m
            j = 1

            do i = 0, m/2 - 1

                i0 = i4_wrap(i, 1, m/2)
                i2 = i4_wrap(i + 1, 1, m/2)

                i1 = i4_wrap(i + m/2, m/2 + 1, m)
                i3 = i4_wrap(i + m/2 + 1, m/2 + 1, m)

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

                i0 = i4_wrap(i, 1, m/2)
                i2 = i4_wrap(i + 1, 1, m/2)

                i1 = i4_wrap(i + m/2, m/2 + 1, m)
                i3 = i4_wrap(i + m/2 + 1, m/2 + 1, m)

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

    return
end subroutine
