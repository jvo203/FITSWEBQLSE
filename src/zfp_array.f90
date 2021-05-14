module zfp_array
    integer(kind=4), parameter :: max_bits = 128
    integer(kind=4), parameter :: fraction_bits = 30
    integer(kind=4), parameter :: EBIAS = 127

    integer(kind=4), dimension(16), parameter :: PERM =&
    & [0, 1, 4, 5, 2, 8, 6, 9, 3, 12, 10, 7, 13, 11, 14, 15]

    integer(kind=4), parameter :: NBMASK = Z'aaaaaaaa'

    ! Golomb encoding
    integer, parameter :: G_M = 10
    integer, parameter :: G_b = 4

    ! Rice encoding
    integer, parameter :: R_k = 3
    integer, parameter :: R_M = 2**R_k

    type zfp_block
        ! a 128-bit bitstream holding a compressed 4x4 real array
        integer(kind=16) :: bitstream
    end type zfp_block
end module zfp_array
