program main
   use differential_evolution
   use omp_lib
   implicit none

   ! make dim a parameter
   integer, parameter :: dim = 10

   integer, allocatable :: seed(:)

   integer :: pop_size = 10*dim ! a recommended population size
   real(float) :: min_val(dim), max_val(dim)
   type(Population) :: pop

   integer :: iter, n

   ! set the min and max values for each dimension to [-10, 10]
   !min_val(:) = -10.0
   !max_val(:) = 10.0

   min_val(:) = -5.12
   max_val(:) = 5.12

   ! randomize the integer seed
   call random_seed(size = n)

   allocate(seed(n))
   call random_seed(get=seed)

   ! initialize the population
   call init_population(pop, pop_size, dim, seed(n), min_val, max_val)

   ! evaluate the population for a number of generations
   do iter = 1, 1000
      call evaluate_population(pop)

      ! print the best cost
      print *, "iter:", iter, "best cost:", pop%best_cost, "best idx:", pop%best_idx, "best genotype:", pop%best(pop%best_idx)%genotype

      call update_population(pop)
   end do

   ! print the final best solution
   print *, "best cost:", pop%best_cost, "best genotype:", pop%best(pop%best_idx)%genotype

   call finalize_population(pop)

   deallocate(seed)

contains

   ! a sum of squares function
   function sum_of_squares(x) result(cost)
      real(float), dimension(:), intent(in) :: x
      real(float) :: cost

      cost = sum(x**2)
   end function sum_of_squares


   ! The Rastrigin function is a non-convex function used as a
   ! performance test problem for optimization algorithms.
   ! see https://en.wikipedia.org/wiki/Rastrigin_function
   function rastrigin(x) result(cost)
      real(float), dimension(:), intent(in) :: x
      real(float) :: cost

      real(float), parameter :: two_pi = 8.0*atan(1.0)
      integer :: i

      cost = 10.0 * size(x)

      do i = 1, size(x)
         cost = cost + x(i)**2 - 10.0*cos(two_pi*x(i))
      end do
   end function rastrigin

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

      !$omp parallel shared(pop) private(i, cost)
      !$omp do
      do i = 1, pop%pop_size
         ! evaluate the current genotype
         ! cost = rosenbrock(pop%curr(i)%genotype)
         ! cost = sum_of_squares(pop%curr(i)%genotype)
         cost = rastrigin(pop%curr(i)%genotype)
         pop%curr(i)%cost = cost

         if (cost .le. pop%best(i)%cost) then
            pop%best(i)%cost = cost
            pop%best(i)%genotype = pop%curr(i)%genotype
         end if

         !$omp critical
         call update_pop_best(pop, cost, i)
         !$omp end critical
      end do
      !$omp end do
      !$omp end parallel

   end subroutine evaluate_population

end program main
