module ast
   implicit none

   interface
      ! void astBegin_( void )
      subroutine astBegin() bind(C, name="astBegin_")
         use, intrinsic :: ISO_C_BINDING
         implicit none
      end subroutine astBegin

      ! void astEnd_( int *status )
      subroutine astEnd( status ) bind(C, name="astEnd_")
         use, intrinsic :: ISO_C_BINDING
         implicit none

         integer(C_INT), intent(out) :: status
      end subroutine astEnd
   end interface

end module ast
