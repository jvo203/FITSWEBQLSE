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
!    Input, real ( kind = 8 ) X(N), the vector to be transformed.
!
!    Output, real ( kind = 8 ) Y(N), the transformed vector.
!
  implicit none

  integer(kind=4) n
  integer(kind=4), parameter :: p = 3

  real(kind=8), dimension(0:p) :: c = (/ &
                                  0.4829629131445341D+00, &
                                  0.8365163037378079D+00, &
                                  0.2241438680420133D+00, &
                                  -0.1294095225512603D+00/)
  integer(kind=4) i
  integer(kind=4) i4_wrap
  integer(kind=4) j
  integer(kind=4) j0
  integer(kind=4) j1
  integer(kind=4) j2
  integer(kind=4) j3
  integer(kind=4) m
  real(kind=8) x(n)
  real(kind=8) y(n)
  real(kind=8) z(n)

  y(1:n) = x(1:n)
  z(1:n) = 0.0D+00

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
!    Input, real ( kind = 8 ) Y(N), the transformed vector.
!
!    Output, real ( kind = 8 ) X(N), the original vector.
!
    implicit none

    integer(kind=4) n
    integer(kind=4), parameter :: p = 3

    real(kind=8), dimension(0:p) :: c = (/ &
                                    0.4829629131445341D+00, &
                                    0.8365163037378079D+00, &
                                    0.2241438680420133D+00, &
                                    -0.1294095225512603D+00/)
    integer(kind=4) i
    integer(kind=4) i0
    integer(kind=4) i1
    integer(kind=4) i2
    integer(kind=4) i3
    integer(kind=4) i4_wrap
    integer(kind=4) j
    integer(kind=4) m
    real(kind=8) x(n)
    real(kind=8) y(n)
    real(kind=8) z(n)

    x(1:n) = y(1:n)
    z(1:n) = 0.0D+00

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
subroutine daub6_matrix(n, a)
