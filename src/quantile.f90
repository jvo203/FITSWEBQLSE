module quantile_mod    ! quantile finder.    median = quantile( size(a)/2, a)
   use, intrinsic :: ieee_arithmetic, only: ieee_quiet_nan, ieee_value
contains
   pure recursive function quantile(k, a) result(value)
      integer, intent(in)  :: k          ! position in array
      real, dimension(:), intent(in)  :: a

      real                              :: value      ! output value of quantile
      integer                           :: j
      real                              :: ak

      if(k .lt. 1 .or. size(a) .lt. 1) then
         value = ieee_value(0.0, ieee_quiet_nan)
         return
      end if

      if (size(a) .eq. 1) then
         value = a(1)
         return
      end if

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
