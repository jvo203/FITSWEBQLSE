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

      ! void ast_annul(AstObject *object)
      subroutine astAnnul( object ) bind(C, name="ast_annul")
         use, intrinsic :: ISO_C_BINDING
         implicit none

         type(C_PTR), value, intent(in) :: object
      end subroutine astAnnul
   end interface

end module ast

====================== BEFORE ! WCS

! AST
      integer(c_int) :: ast_status
      type(C_PTR) :: wcsinfo

====================== BEFORE ! WCSLIB

call astBegin

      wcsinfo = astReadHeader(item%hdr)

      ! check if the WCS is valid
      ! if (c_associated(wcsinfo)) then
      call astPix2Sky( wcsinfo, cx, cy, lng, lat )

      ! check if lng and lat are not NaN (i.e. the WCS is valid)
      if (.not. ieee_is_nan(lng) .and. .not. ieee_is_nan(lat)) then
         ! beam_width
         call astPix2Sky(wcsinfo, cx - rx, cy, ra1, dec1)
         call astPix2Sky(wcsinfo, cx + rx, cy, ra2, dec2)

         ! convert ra, dec from degrees to radians
         ra1 = ra1*deg2rad
         dec1 = dec1*deg2rad

         ra2 = ra2*deg2rad
         dec2 = dec2*deg2rad

         ! convert from radians to degrees
         beam_width = AngularDistance(ra1, dec1, ra2, dec2) * rad2deg

         ! beam_height
         call astPix2Sky(wcsinfo, cx, cy - ry, ra1, dec1)
         call astPix2Sky(wcsinfo, cx, cy + ry, ra2, dec2)

         ! convert ra, dec from degrees to radians
         ra1 = ra1*deg2rad
         dec1 = dec1*deg2rad

         ra2 = ra2*deg2rad
         dec2 = dec2*deg2rad

         ! convert from radians to degrees
         beam_height = AngularDistance(ra1, dec1, ra2, dec2) * rad2deg

         print *, 'lng: ', lng, ', lat: ', lat, ', beam_width: ', beam_width, ', beam_height: ', beam_height
      end if

      call astAnnul(wcsinfo)
      call astEnd (ast_status)
