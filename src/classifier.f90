module classifier
    implicit none
contains
    function classify_histogram(Slots, n)
        integer :: classify_histogram, tone_mapping
        integer, intent(in) :: n
        real, dimension(n), intent(in) :: Slots

        classify_histogram = -1

        if (n .ne. 1024) return

    end function classify_histogram
end module classifier
