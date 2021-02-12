module fixed_array
    implicit none

    type fixed_block
        integer(kind=1) :: common_exp
        integer(kind=1), dimension(4, 4) :: mantissa
    end type fixed_block
end module fixed_array
