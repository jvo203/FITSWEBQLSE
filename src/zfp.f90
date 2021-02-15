module zfp
    use zFORp
    use, intrinsic :: iso_c_binding
    implicit none

contains
! Fortran convenience subroutines

    subroutine compress_pixels(pixels, compressed)
        use, intrinsic :: iso_c_binding
        implicit none

        ! inputs
        real(kind=4), dimension(:, :), contiguous, target, intent(in) :: pixels

        ! the output
        character(kind=c_char), allocatable, intent(out) :: compressed(:)

        ! internal variables
        integer :: nx, ny
        character(kind=c_char), allocatable, target :: buffer(:)

        nx = ubound(pixels, 1)
        ny = ubound(pixels, 2)

        print *, 'nx = ', nx, ', ny = ', ny

    end subroutine compress_pixels

end module zfp
