! Author:  Philipp Engel
! Licence: ISC
module util
   use, intrinsic :: iso_c_binding
   use :: unix_pthread
   implicit none

contains
   recursive subroutine foo(arg) bind(c)
      type(c_ptr), intent(in), value :: arg   ! Client data.
      real :: r

      if (.not. c_associated(arg)) return

      ! get a random interval between 1 and 10 seconds
      call random_number(r)
      call sleep(nint(10*r))

      print *, "foo"

   end subroutine foo
end module util

program main
   use, intrinsic :: iso_c_binding
   use :: unix_pthread
   use :: util
   implicit none

   integer            :: rc
   type(c_pthread_t)  :: thread

   print '(a)', 'Starting a thread ...'

   rc = c_pthread_create(thread=thread, &
      attr=c_null_ptr, &
      start_routine=c_funloc(foo), &
      arg=c_null_ptr)

   print *, "c_pthread_create::rc", rc


   print '(a)', 'Joining a thread ...'

   rc = c_pthread_join(thread, c_null_ptr)
   print *, "c_pthread_join::rc", rc

end program main
