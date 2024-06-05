module ast
   implicit none

   interface
      ! void astBegin_( void )
      subroutine AST_BEGIN() bind(C, name="astBegin_")
         use, intrinsic :: ISO_C_BINDING
         implicit none
      end subroutine AST_BEGIN

      ! void astEnd_( int *status )
      subroutine AST_END( status ) bind(C, name="astEnd_")
         use, intrinsic :: ISO_C_BINDING
         implicit none

         integer(C_INT), intent(out) :: status
      end subroutine AST_END
   end interface

end module ast
