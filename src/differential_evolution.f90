module differential_evolution
   use,intrinsic :: iso_fortran_env
   use mt19937_64   

   implicit none

   private

   integer,parameter :: float = real32  ! single precision real kind [4 bytes].
   ! integer,parameter :: float = real64  ! double precision real kind [8 bytes].

   type, public Individual
      real(float), dimension(:), allocatable :: genotype ! the genotype of the individual solution
      real(float) :: cost ! the cost of the individual solution

      ! control parameters
      real(float) :: cr = 0.5_float
      real(float) :: f = 0.5_float
   end type Individual

   type, public :: Population
      type(Individual), dimension(:), allocatable :: curr, best ! current and best individuals      

      integer :: pop_size = 0 ! population size
      integer :: best_idx = 0 ! index of the best individual

      real(float) :: best_cost = huge(0.0_float) ! cost of the best individual
      integer :: num_cost_evals = 0 ! number of cost evaluations
      integer :: dim = 0 ! dimension of the problem

      type(mt19937) :: rand ! a random number generator

      ! a user-supplied procedure
      ! procedure(cost_func), pointer, nopass :: user_cost => null() ! a fitness function
   end type Population

   abstract interface

      subroutine cost_func(pop, x, cost)
         import :: float, Population
         implicit none

         type(Population), intent(inout) :: pop ! the population
         real(float), dimension(:), intent(in) :: x ! a chromosome
         real(float), intent(out) :: cost ! a fitness value
      end subroutine cost_func

   end interface


contains

end module differential_evolution
