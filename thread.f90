module util
   use, intrinsic :: iso_c_binding
   implicit none

   interface
      ! extern int my_pthread_create(pthread_t *thread, void *(*start_routine)(void *), void *arg)
      function my_pthread_create(start_routine, arg, rc) bind(c, name='my_pthread_create')
         import :: c_int, c_ptr, c_funptr
         implicit none
         type(c_funptr), intent(in), value :: start_routine
         type(c_ptr), intent(in), value :: arg
         type(c_ptr) :: my_pthread_create
         integer(c_int) :: rc
      end function my_pthread_create

      ! extern int my_pthread_detach(pthread_t thread)
      function my_pthread_detach(thread) bind(c, name='my_pthread_detach')
         import :: c_int, c_ptr
         implicit none
         ! type(my_pthread_t), intent(in), value :: thread
         type(c_ptr), intent(in), value :: thread
         integer(kind=c_int)                  :: my_pthread_detach
      end function my_pthread_detach

      ! extern int my_pthread_join(pthread_t thread)
      function my_pthread_join(thread) bind(c, name='my_pthread_join')
         import :: c_int, c_ptr
         implicit none
         ! type(my_pthread_t), intent(in), value :: thread
         type(c_ptr), intent(in), value :: thread
         integer(kind=c_int)                  :: my_pthread_join
      end function my_pthread_join
   end interface

contains
   recursive subroutine foo(arg) bind(c)
      type(c_ptr), intent(in), value :: arg   ! Client data.
      real :: r

      ! get a random interval between 1 and 10 seconds
      call random_number(r)
      call sleep(nint(10*r))

      print *, "foo"

   end subroutine foo
end module util

program main
   use, intrinsic :: iso_c_binding
   ! use :: unix_pthread
   use :: util
   implicit none

   integer            :: rc
   type(c_ptr) :: tid

   print '(a)', 'Starting a thread ...'

   tid = my_pthread_create(start_routine=c_funloc(foo), arg=c_null_ptr, rc=rc)

   print *, "my_pthread_create::rc", rc
   print *, "my_pthread_create::tid", tid

   print '(a)', 'Joining a thread ...'

   rc = my_pthread_join(tid)
   print *, "my_pthread_join::rc", rc

end program main
