program main
   use differential_evolution
   use omp_lib
   implicit none

   ! make dim a parameter
   integer, parameter :: dim = 2

   integer, allocatable :: seed(:)

   integer :: pop_size = 20
   real(float) :: min_val(dim), max_val(dim)
   type(Population) :: pop

   integer :: i, n

   ! set the min and max values for each dimension to [-10, 10]
   min_val(:) = -10.0
   max_val(:) = 10.0

   ! randomize the integer seed
   call random_seed(size = n)

   allocate(seed(n))
   call random_seed(get=seed)

   print *, "seed:", seed

   ! initialize the population
   call init_population(pop, pop_size, dim, seed(n), min_val, max_val)

   ! evaluate the population for a number of generations
   do i = 1, 1
      call evaluate_population(pop)

      ! print the best cost
      print *, "i:", i, "best cost:", pop%best_cost, "best idx:", pop%best_idx, "best genotype:", pop%best(pop%best_idx)%genotype

      call update_population(pop)
   end do

contains

   ! Rosenbrock function
   ! #Reference
   ! * [Rosenbrock function](http://en.wikipedia.org/wiki/Rosenbrock_function)
   function rosenbrock(x) result(cost)
      real(float), dimension(:), intent(in) :: x
      real(float) :: cost

      real(float),parameter :: one     = 1.0_float
      real(float),parameter :: hundred = 100.0_float

      cost = (one-x(1))**2 + hundred*(x(2)-x(1)**2)**2
   end function rosenbrock

   subroutine evaluate_population(pop)
      type(Population), intent(inout) :: pop

      real(float) :: cost
      integer :: i

      do i = 1, pop%pop_size
         ! evaluate the current genotype
         cost = rosenbrock(pop%curr(i)%genotype)
         pop%curr(i)%cost = cost

         ! update the best genotype
         if (cost .lt. pop%best_cost) then
            pop%best(i)%genotype = pop%curr(i)%genotype
            pop%best_cost = cost
            pop%best_idx = i
         end if
      end do

   end subroutine evaluate_population

end program main
