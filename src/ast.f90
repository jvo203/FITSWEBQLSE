module ast
   use, intrinsic :: ISO_C_BINDING

   implicit none

! redefine the C_NULL_PTR constant as AST_NULL parameter
   type(c_ptr), parameter :: AST_NULL = C_NULL_PTR

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

      ! AstFitsChan *astFitsChanId_( const char *(* source)( void ),
      ! void (* sink)( const char * ),
      ! const char *options, ... )
      type(C_PTR) function astFitsChan( source, sink, options ) bind(C, name="astFitsChanId_")
         use, intrinsic :: ISO_C_BINDING
         implicit none

         type(C_FUNPTR), intent(in) :: source
         type(C_FUNPTR), intent(in) :: sink
         character(C_CHAR), dimension(*), intent(in) :: options
      end function astFitsChan

      ! void astShow_( AstObject *, int * );
      subroutine astShow( obj, status ) bind(C, name="astShow_")
         use, intrinsic :: ISO_C_BINDING
         implicit none

         type(C_PTR), value, intent(in) :: obj
         integer(C_INT), intent(out) :: status
      end subroutine astShow

      ! AstFrameSet *ast_read_header(const char *header)
      type(C_PTR) function astReadHeader( header ) bind(C, name="ast_read_header")
         use, intrinsic :: ISO_C_BINDING
         implicit none

         character(C_CHAR), dimension(*), intent(in) :: header
      end function astReadHeader

      ! void astPix2Sky(AstFrameSet *wcsinfo, float x, float y, double *ra, double *dec)
      subroutine astPix2Sky( wcsinfo, x, y, ra, dec ) bind(C, name="astPix2Sky")
         use, intrinsic :: ISO_C_BINDING
         implicit none

         type(C_PTR), value, intent(in) :: wcsinfo
         real(C_FLOAT), value, intent(in) :: x, y
         real(C_DOUBLE), intent(out) :: ra, dec
      end subroutine astPix2Sky
   end interface

end module ast
