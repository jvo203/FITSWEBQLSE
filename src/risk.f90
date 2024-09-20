module risk
   implicit none

contains

   ! by Milan Curcic
   pure real function average(x)
      real, intent(in) :: x(:)
      average = sum(x) / size(x)
   end function average

   ! by Milan Curcic
   pure real function std(x)
      real, intent(in) :: x(:)
      std = sqrt(average((x - average(x))**2))
   end function std

   pure real function stdm(x, m)
      real, intent(in) :: x(:), m
      stdm = sqrt(average((x - m)**2))
   end function stdm

   pure function std2(arr) result(std_dev)
      implicit none

      real, intent(in), dimension(:) :: arr
      real ::std_dev
      real::variance, avg
      integer::n, i

      n = size(arr)
      avg = sum(arr)/n
      variance = 0.0

      do i = 1, n
         variance = variance + (arr(i) - avg)**2
      end do

      variance = variance/n
      std_dev = sqrt(variance)

   end function std2

end module risk
