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

subroutine rotate_hds_image_spectrum_y(pixels, mask, x0, y0, theta, gamma, yspec, ymask)
   use omp_lib
   implicit none

   real(c_float), dimension(:,:), intent(in), target :: pixels
   logical(kind=c_bool), dimension(:,:), intent(in), target :: mask
   real(c_float), intent(in) :: x0, y0, theta, gamma

   ! X and Y spectra
   real(c_float), allocatable, intent(out) :: yspec(:)
   logical(kind=c_bool), allocatable, intent(out) :: ymask(:)

   ! the viewport and the mask
   real(c_float), allocatable, target :: outspec(:)
   logical(kind=c_bool), allocatable, target :: outmask(:)

   integer :: dimx, dimy, i, j, tx, ty, max_threads
   integer :: ymin, ymax ! non-NaN spectrum bounds
   logical(kind=c_bool), allocatable, target :: valid(:)
   integer :: x1, x2 ! the Y spectrum band

   real :: qx, qy, val
   integer :: count

   dimx = size(pixels, 1)
   dimy = size(pixels, 2)

   ! get #physical cores (ignore HT)
   max_threads = get_max_threads()

   x1 = int(nint(x0-gamma))
   x2 = int(nint(x0+gamma))

   ! debugging override:
   !x1 = x0-1
   !x2 = x0+1

   ! check the bounds
   x1 = max(1, x1)
   x2 = min(dimx, x2)

   print *, 'rotate_hds_image_spectrum x1,x2 band limits:', x1, x2

   ! a full wide-band Y spectrum
   allocate(outspec(dimy))
   allocate(outmask(dimy))
   allocate(valid(dimy))

   ! go through the i, j pixels and rotate them around the point (x0, y0) by the angle theta
   !$omp parallel shared(outspec, outmask, valid, pixels, mask, x1, x2) private(i, count, val, qx, qy, tx, ty)&
   !$omp& NUM_THREADS(max_threads)
   !$omp do schedule(dynamic, 4)
   do j = 1, dimy
      ! sum up each row
      count = 0
      val = 0.0

      outspec(j) = 0.0
      outmask(j) = .false.
      valid(j) = .false.

      !$omp simd
      do i = x1, x2
         call rotate(real(i), real(j), x0, y0, theta, qx, qy)
         tx = int(nint(qx))
         ty = int(nint(qy))

         ! check if tx and ty are within the bounds
         if ((tx .lt. 1) .or. (tx .gt. dimx) .or. (ty .lt. 1) .or. (ty .gt. dimy)) cycle

         if(mask(tx, ty)) then
            outmask(j) = .true.
            val = val + pixels(tx, ty)
            count = count + 1
         end if

         valid(j) = .true. ! there is at least one 'in-range' pixel in the row
      end do

      if (count .gt. 0) outspec(j) = val / real(count)
   end do
   !$omp end do
   !$omp end parallel

   call hds_image_spectrum_y(x1 - 1, x2 - 1, x0 - 1.0, y0 - 1.0, theta, c_loc(pixels), c_loc(mask), dimx, dimy,&
   & c_loc(outspec), c_loc(outmask), c_loc(valid))

   ! find the non-NaN bounds
   ymin = 1
   ymax = dimy

   do i = 1, dimy
      if (valid(i)) then
         ymin = i
         exit
      end if
   end do

   do i = dimy, 1, -1
      if (valid(i)) then
         ymax = i
         exit
      end if
   end do

   yspec = outspec(ymin:ymax)
   ymask = outmask(ymin:ymax)

   ! print the final count
   print *, 'rotate_hds_image_spectrum_y count:', size(yspec)

   ! print the first 10 values of yspec and ymask
   ! print *, '  yspec:', yspec(1:10)
   ! print *, '  ymask:', ymask(1:10)

end subroutine rotate_hds_image_spectrum_y

subroutine DownsizePolarization(pixels, mask, width, height, pol_xmin, pol_ymin, pol_xmax, pol_ymax,&
& pol_target, pol_intensity, pol_angle)
   implicit none

   real(c_float), dimension(:, :, :), intent(in), contiguous, target :: pixels
   logical(kind=c_bool), dimension(:, :), intent(in), contiguous, target :: mask
   integer, intent(in) :: width, height
   integer, intent(in) :: pol_xmin, pol_ymin, pol_xmax, pol_ymax, pol_target
   real(kind=c_float), dimension(:,:), allocatable, intent(out) :: pol_intensity, pol_angle

   integer :: xmin, xmax, ymin, ymax, dimx, dimy
   integer :: max_threads, max_planes, i, j, ii, jj

   integer :: range, count, min_count, total_count
   real :: range_x, range_y

   real :: tmp, tmpA, tmpI, tmpQ, tmpU, tmpV
   real :: intensity, angle
   integer :: x0, y0

   ! SPMD C interface
   real(kind=c_float), target :: res(2)
   integer(kind=c_int) :: c_count, c_offset, c_stride

   max_planes = size(pixels, 3)
   if (max_planes .lt. 3) return

   xmin = max(lbound(pixels, 1), pol_xmin)
   xmax = min(ubound(pixels, 1), pol_xmax)
   ymin = max(lbound(pixels, 2), pol_ymin)
   ymax = min(ubound(pixels, 2), pol_ymax)

   print *, 'DownsizePolarization: xmin:', xmin, 'xmax:', xmax, 'ymin:', ymin, 'ymax:', ymax, 'max_planes:', max_planes

   c_stride = size(pixels, 1)
   c_offset = size(pixels, 1) * size(pixels, 2)
   print *, 'DownsizePolarization: c_stride:', c_stride, 'c_offset:', c_offset

   dimx = abs(xmax - xmin) + 1
   dimy = abs(ymax - ymin) + 1

   range_x = real(dimx)/real(pol_target)
   range_y = real(dimy)/real(pol_target)

   range = max(1, floor(max(range_x, range_y)))
   min_count = nint(0.75*range**2)

   ! print the width X height of the input pixels and mask
   print *, 'DownsizePolarization: width:', width, 'height:', height, 'dimx:', dimx, 'dimy:', dimy,&
   & 'range:', range, 'min_count:', min_count

   ! allocate the output arrays
   allocate (pol_intensity(1+(xmax - xmin)/range, 1+(ymax - ymin)/range))
   allocate (pol_angle(1+(xmax - xmin)/range, 1+(ymax - ymin)/range))

   ! clear the output arrays
   pol_intensity = 0.0
   pol_angle = 0.0

   ! re-set the counter
   total_count = 0

   ! get #physical cores (ignore HT)
   max_threads = get_max_threads()

   ! loop over the pixels and mask
   !$omp PARALLEL DEFAULT(SHARED) SHARED(pol_intensity, pol_angle, min_count, range)&
   !$omp& SHARED(pixels, mask) PRIVATE(i, j, tmp, tmpA, tmpI, tmpQ, tmpU, tmpV)&
   !$omp& PRIVATE(x0, y0, intensity, angle, count, c_count, res, ii, jj)&
   !$omp& REDUCTION(+:total_count)&
   !$omp& NUM_THREADS (max_threads)
   !$omp DO
   do j = ymin, ymax, range
      do i = xmin, xmax, range
         ! print *, 'DownsizePolarization: i:', i, 'j:', j, 'range:', range, 'xmax:', xmax, 'ymax:', ymax

         intensity = 0.0
         angle = 0.0
         count = 0

         do jj = j, min(j + range - 0, ymax)
            do ii = i, min(i + range - 0, xmax)
               if (mask(ii, jj)) then
                  tmpI = pixels(ii, jj, 1)
                  tmpQ = pixels(ii, jj, 2)
                  tmpU = pixels(ii, jj, 3)

                  if (abs(tmpQ) .le. epsilon(tmpQ) .and. abs(tmpU) .le. epsilon(tmpU)) then
                     tmpA = ieee_value(0.0, ieee_quiet_nan)
                  else
                     tmpA = 0.5*atan2(tmpU, tmpQ) ! polarisation angle
                  end if

                  if (max_planes .gt. 3) then
                     tmpV = pixels(ii, jj, 4)

                     if (abs(tmpI) .le. epsilon(tmpI)) then
                        tmp = ieee_value(0.0, ieee_quiet_nan)
                     else
                        tmp = sqrt(tmpQ**2 + tmpU**2 + tmpV**2)/tmpI ! total intensity
                     end if
                  else
                     if (abs(tmpI) .le. epsilon(tmpI)) then
                        tmp = ieee_value(0.0, ieee_quiet_nan)
                     else
                        tmp = sqrt(tmpQ**2 + tmpU**2)/tmpI ! linear intensity
                     end if
                  end if

                  if ((.not. ieee_is_nan(tmpA)) .and. (.not. ieee_is_nan(tmp))) then
                     intensity = intensity + tmp
                     angle = angle + tmpA
                     count = count + 1
                  end if
               end if
            end do
         end do

         ! SPMD C
         if (max_planes .gt. 3) then
            c_count = polarisation_simd_4(c_loc(pixels), c_loc(mask), c_offset, c_stride, range,&
            & i-1, j-1, xmax-1, ymax-1, c_loc(res))
         else
            c_count = polarisation_simd_3(c_loc(pixels), c_loc(mask), c_offset, c_stride, range,&
            & i-1, j-1, xmax-1, ymax-1, c_loc(res))
         end if

         if (c_count .ge. min_count) then
            !intensity = intensity/real(count)
            !angle = angle/real(count)
            intensity = res(1)
            angle = res(2)

            ! at first 0-based indexing, remove the offset and the step, then make i and j 1-based array indices
            x0 = 1 + (i - xmin)/range
            y0 = 1 + (j - ymin)/range

            ! fill-in the output arrays
            if (x0 .ge. 1 .and. x0 .le. size(pol_intensity, 1) .and. y0 .ge. 1 .and. y0 .le. size(pol_intensity, 2)) then
               pol_intensity(x0, y0) = intensity
               pol_angle(x0, y0) = angle

               total_count = total_count + 1
               ! print *, 'DownsizePolarization: x:', x0, 'y:', y0, 'intensity:', intensity, 'angle:', angle
            end if
         end if

      end do
   end do
   !$omp END DO
   !$omp END PARALLEL

   print *, 'DownsizePolarization: total_count:', total_count, 'max_threads:', max_threads

   !print *, pol_intensity(1:10, 1:10)
   !print *, pol_angle(1:10, 1:10)

end subroutine DownsizePolarization
