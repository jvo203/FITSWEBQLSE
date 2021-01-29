module classifier
    implicit none
contains
    function classify_histogram(Slots, n)
        integer :: classify_histogram, tone_mapping
        integer, intent(in) :: n
        real, dimension(n), intent(in) :: Slots

        tone_mapping = -1
        classify_histogram = tone_mapping

        if (n .ne. 1024) return

    end function classify_histogram
end module classifier
