module quantile_mod    ! quantile finder.    median = quantile( size(a)/2, a)
contains
    pure recursive function quantile(k, a) result(value)
        integer, intent(in)  :: k          ! position in array
        real, dimension(:), intent(in)  :: a

        real                              :: value      ! output value of quantile
        integer                           :: j
        real                              :: ak

        ak = a(k)
        j = count(a < ak)                              ! how many a(:) < ak

        if (j .ge. k) then
            value = quantile(k, pack(a, a < ak))
        else
            j = count(a > ak) + k - size(a)

            if (j .gt. 0) then
                value = quantile(j, pack(a, a > ak))
            else
                value = ak
            end if
        end if
    end function quantile
end module quantile_mod
