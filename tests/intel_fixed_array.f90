program main
   use, intrinsic :: iso_c_binding
   use, intrinsic :: ieee_arithmetic
   implicit none

   ! 3D data cube dimensions
   integer, parameter :: nx = 420, ny = 420, nz = 1908

   ! 3D data cube
   real(kind=4), dimension(:,:,:), allocatable :: data

   print *, "NEW INTEL FORTRAN (IFX) TEST"

   ! dynamically allocate the data
   allocate(data(nx,ny,nz))

   ! fill it with random data
   call random_number(data)

   ! print some elements
   print *, data(1:5,1:5,1:5)

contains

end program main
