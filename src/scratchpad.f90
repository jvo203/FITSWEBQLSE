! angle = theta*180/3.1415926535897932384626433832795
 ! angle1 = int(nint(angle)) - 10
 ! angle2 = int(nint(angle)) + 10
 ! theta = find_angle(b, w, gamma, x1 + mu - 1, item%pixels, item%mask, angle1, angle2) ! using the whole image
 ! print *, 'theta angle [rad]:', theta , ', degrees:', theta*180/3.1415926535897932384626433832795

 ! refine the parameters with a gradient descent
 ! call gradient_descent(b, w, gamma, mu, theta, view_pixels, view_mask, 0.1, 100, 0.001, b, w, gamma, mu, theta)
 ! print *, 'grad.desc. theta [rad]:', theta , ', degrees:', theta*180/3.1415926535897932384626433832795, 'mu:', mu

 ! and again refine the angle
angle = theta*180/3.1415926535897932384626433832795
angle1 = int(nint(angle)) - 10
angle2 = int(nint(angle)) + 10
 ! theta = find_angle(b, w, gamma, mu, view_pixels, view_mask, angle1, angle2)


 ! find the gradient of the RMS error function with respect to theta, b, w, gamma and mu
function gradient(b, w, alpha, beta, mu, db, dw, dalpha, dbeta, dmu, view, mask) result(rmse)
   use omp_lib
   implicit none

   real(c_float), intent(in) :: b, w, alpha, beta, mu
   real(c_float), intent(out) :: db, dw, dalpha, dbeta, dmu
   real(c_float), dimension(:,:), intent(in) :: view
   logical(kind=c_bool), dimension(:,:), intent(in) :: mask

   integer :: i, j, dimx, dimy, count, max_threads
   real :: rmse, x0, y0, rotated

   rmse = 0.0
   count = 0

   db = 0.0
   dw = 0.0
   dalpha = 0.0
   dbeta = 0.0
   dmu = 0.0

   dimx = size(view, 1)
   dimy = size(view, 2)

   x0 = mu
   y0 = real(dimy)/2

   ! get #physical cores (ignore HT)
   max_threads = get_max_threads()

   ! use OpenMP to parallelize the loop, reducing the gradients

   !$omp PARALLEL DEFAULT(SHARED) SHARED(view, mask, dimx, dimy, x0, y0, b, w, alpha, beta)&
   !$omp& PRIVATE(i, j, rotated)&
   !$omp& REDUCTION(+:count, rmse, db, dw, dalpha, dbeta, dmu)&
   !$omp& NUM_THREADS(max_threads)
   !$omp DO
   do j = 1, dimy
      do i = 1, dimx
         if (mask(i, j)) then
            ! call an integrated function: rotation + gradients
            rotated = rmse_gradient(real(i), real(j), x0, y0, b, w, alpha, beta, view(i, j), db, dw, dalpha, dbeta, dmu)

            rmse = rmse + (rotated - view(i, j))**2
            count = count + 1
         end if
      end do
   end do
   !$omp END DO
   !$omp END PARALLEL

   if (count .gt. 0) then
      rmse = sqrt(rmse/real(count))
      db = db / real(count)
      dw = dw / real(count)
      dalpha = dalpha / real(count)
      dbeta = dbeta / real(count)
      dmu = dmu / real(count)
   end if
end function gradient

subroutine gradient_descent(b, w, gamma, mu, theta, view, mask, eta, max_iter, tol, b0, w0, gamma0, mu0, theta0)
   implicit none

   real(c_float), intent(inout) :: b, w, gamma, mu, theta
   real(c_float), intent(in) :: eta, tol
   real(c_float), dimension(:,:), intent(in) :: view
   logical(kind=c_bool), dimension(:,:), intent(in) :: mask
   integer(c_int), intent(in) :: max_iter
   real(c_float), intent(in) :: b0, w0, gamma0, mu0, theta0

   real(c_float) :: db, dw, alpha, newalpha, dalpha, beta, dbeta, dmu, newmu
   real(c_float) :: rmse, rmse1
   integer :: iter
   real :: dim

   dim = real(size(view, 1))

   b = b0
   w = w0
   alpha = log(gamma0)
   beta = tan(theta0)
   mu = mu0

   do iter = 1, max_iter
      rmse = gradient(b, w, alpha, beta, mu, db, dw, dalpha, dbeta, dmu, view, mask)

      print *, 'iter:', iter, 'b:', b, 'w:', w, 'alpha:', alpha, 'beta', beta, 'theta:',&
      &theta, 'gamma:', exp(alpha), atan(beta), 'mu:', mu, 'rmse:', rmse

      ! print out the gradients too
      print *, 'db:', db, 'dw:', dw, 'dalpha:', dalpha, 'dbeta:', dbeta, 'dmu:', dmu

      ! if (abs(db) .lt. tol .and. abs(dw) .lt. tol .and. abs(dgamma) .lt. tol .and. abs(dtheta) .lt. tol .and. abs(dmu) .lt. tol) exit

      ! update the parameters

      ! b = b + eta * db
      ! w = w + eta * dw

      ! gamma = gamma + eta * dgamma
      newalpha = alpha + eta * dalpha

      ! range-limit between log(0.001) and log(5.0)
      if (newalpha .le. 1.6094379124341003 .and. newalpha .ge. -6.907755278982137) alpha = newalpha

      newmu = mu + 0.1*eta * dmu

      ! range-limit to between mu0 - 10 and mu0 + 10
      if (newmu .le. min(dim, mu0 + 10.0) .and. newmu .ge. max(1.0, mu0 - 10.0)) mu = max(1.0,min(dim, newmu))

      ! beta = beta + 0.1 * eta * dbeta
   end do

   gamma = exp(alpha)
   theta = atan(beta)
end subroutine gradient_descent

 ! a point (i, j) rotated by a theta angle around the point (x0, y0)
function rmse_gradient_theta(i, j, x0, y0, theta, b, w, alpha, t, db, dw, dalpha, dtheta, dx0) result(y)
   implicit none

   real, intent(in) :: i, j, x0, y0, theta, b, w, alpha, t
   real, intent(inout) :: db, dw, dalpha, dtheta, dx0

   real :: y, inner, inner2, gamma, peak, error

   ! intermediate variables
   inner = (i - x0)*cos(theta) - (j - y0)*sin(theta)
   inner2 = inner**2
   gamma = exp(alpha)
   peak = exp(-gamma * inner2)

   ! output and error
   y = b + w * peak
   error = y - t

   db = db + error
   dw = dw + error * peak
   dalpha = dalpha - error * w * peak * inner2 * gamma
   dtheta = dtheta + error * 2.0 * gamma * w * peak * inner * ((j - y0)*cos(theta) + (i - x0)*sin(theta))
   dx0 = dx0 + error * 2.0 * gamma * w * peak * inner * cos(theta)

end function rmse_gradient_theta

 ! a point (i, j) rotated by a theta angle around the point (x0, y0)
function rmse_gradient(i, j, x0, y0, b, w, alpha, beta, t, db, dw, dalpha, dbeta, dx0) result(y)
   implicit none

   real, intent(in) :: i, j, x0, y0, b, w, alpha, beta, t
   real, intent(inout) :: db, dw, dalpha, dbeta, dx0

   real :: y, inner, inner2, gamma, peak, error, sintheta, costheta, dtheta, betasqrt

   ! trigonometric functions, assuming theta = arctan(beta)
   betasqrt = sqrt(1.0 + beta**2)
   sintheta = beta / betasqrt
   costheta = 1.0 / betasqrt

   ! intermediate variables
   inner = (i - x0)*costheta - (j - y0)*sintheta
   inner2 = inner**2
   gamma = exp(alpha)
   peak = exp(-gamma * inner2)

   ! output and error
   y = b + w * peak
   !error = y - t
   error = t ! maximise the correlation instead

   db = db + error
   dw = dw + error * peak
   dalpha = dalpha - error * w * peak * inner2 * gamma

   dtheta = - 2.0 * gamma * w * peak * inner ! * ((j - y0)*costheta + (i - x0)*sintheta)
   dbeta = dbeta + error * dtheta * ( (j - y0) * ((beta**2)/(betasqrt**3) - costheta) - (i - x0)*beta/(betasqrt**3))

   dx0 = dx0 + error * 2.0 * gamma * w * peak * inner * costheta

end function rmse_gradient

 ! a point (i, j) rotated by a theta angle around the point (x0, y0)
pure function rmse_derive(i, j, x0, y0, b, w, gamma, theta, t) result(dtheta)
   implicit none

   real, intent(in) :: i, j, x0, y0, b, w, gamma, theta, t

   real :: y, inner, inner2, peak, error, dtheta

   ! intermediate variables
   inner = (i - x0)*cos(theta) - (j - y0)*sin(theta)
   inner2 = inner**2
   peak = exp(-gamma * inner2)

   ! output, error
   y = b + w * peak
   error = y - t

   ! derivative
   dtheta = error * 2.0 * gamma * w * peak * inner * ((j - y0)*cos(theta) + (i - x0)*sin(theta))

end function rmse_derive

subroutine rotate_hds_image_spectrum_x(pixels, mask, x0, y0, theta, gamma, xspec, xmask)
   implicit none

   real(c_float), dimension(:,:), intent(in), target :: pixels
   logical(kind=c_bool), dimension(:,:), intent(in), target :: mask
   real(c_float), intent(in) :: x0, y0, theta, gamma

   ! X and Y spectra
   real(c_float), allocatable, intent(out) :: xspec(:)
   logical(kind=c_bool), allocatable, intent(out) :: xmask(:)

   ! the intermediate spectrum and the mask
   real(c_float), allocatable, target :: outspec(:)
   logical(kind=c_bool), allocatable, target :: outmask(:)

   integer :: dimx, dimy, i, tx, ty
   integer :: xmin, xmax ! non-NaN spectrum bounds
   logical(kind=c_bool), allocatable, target :: valid(:)
   real :: qx, qy

   dimx = size(pixels, 1)
   dimy = size(pixels, 2)

   allocate(outspec(dimx))
   allocate(outmask(dimx))
   allocate(valid(dimx))

   ! go through the X axis pixels, rotate them around the point (x0, y0) by the angle theta
   do i = 1, dimx
      call rotate(real(i), y0, x0, y0, theta, qx, qy)
      tx = int(nint(qx))
      ty = int(nint(qy))

      ! check if tx and ty lie within the 2D image bounds
      if ((tx .lt. 1) .or. (tx .gt. dimx) .or. (ty .lt. 1) .or. (ty .gt. dimy)) then
         outspec(i) = 0.0
         valid(i) = .false.
         outmask(i) = .false.
      else
         outspec(i) = pixels(tx, ty)
         outmask(i) = mask(tx, ty)
         valid(i) = .true.
      end if
   end do

   ! print the outspec and outmask arrays
   print *, 'outspec:', outspec(1:100)
   !print *, 'outmask:', outmask

   call hds_image_spectrum_x(x0 - 1.0, y0 - 1.0, theta, c_loc(pixels), c_loc(mask), dimx, dimy,&
   & c_loc(outspec), c_loc(outmask), c_loc(valid))

   print *, 'outspec:', outspec(1:100)
   !print *, 'outmask:', outmask

   ! find the non-NaN X min/max bounds
   xmin = 1
   xmax = dimx

   do i = 1, dimx
      if (valid(i)) then
         xmin = i
         exit
      end if
   end do

   do i = dimx, 1, -1
      if (valid(i)) then
         xmax = i
         exit
      end if
   end do

   xspec = outspec(xmin:xmax)
   xmask = outmask(xmin:xmax)

end subroutine rotate_hds_image_spectrum_x
