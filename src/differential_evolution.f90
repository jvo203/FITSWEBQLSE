module differential_evolution
   use,intrinsic :: iso_fortran_env
   use mt19937_64
   use omp_lib

   implicit none

   private

   integer,parameter :: float = real32  ! single precision real kind [4 bytes].
   ! integer,parameter :: float = real64  ! double precision real kind [8 bytes].

   type Individual
      real(float),dimension(:),allocatable :: params
      real(float) :: cost ! the cost of the individual solution

      ! control parameters
      real(float) :: cr = 0.5_float
      real(float) :: f = 0.5_float
   end type Individual

contains

end module differential_evolution
