module classifier
    implicit none
contains
    function classify_histogram(Slots, n)
        integer :: classify_histogram, tone_mapping
        integer, intent(in) :: n
        real, dimension(0:n - 1), intent(in) :: Slots

        tone_mapping = -1

        classify_histogram = tone_mapping
    end function classify_histogram
end module classifier
