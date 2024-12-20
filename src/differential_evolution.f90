module differential_evolution
   use,intrinsic :: iso_fortran_env
   use mt19937_64

   implicit none

   public

   integer,parameter :: float = real32  ! single precision real kind [4 bytes].
   ! integer,parameter :: float = real64  ! double precision real kind [8 bytes].

   real(float), parameter :: cr_change_probability = 0.1_float
   real(float), parameter :: f_change_probability = 0.1_float

   type Individual
      real(float), dimension(:), pointer :: genotype ! the genotype of the individual solution
      real(float) :: cost = huge(0.0_float) ! the cost of the individual solution

      ! control parameters
      real(float) :: cr = 0.0_float
      real(float) :: f = 0.0_float

   contains
      ! a finalizer to release the pointer memory
      final :: finalize_individual
   end type Individual

   type :: Population
      type(Individual), dimension(:), allocatable :: curr, best ! current and best individuals

      integer :: pop_size = 0 ! population size
      integer :: best_idx = 0 ! index of the best individual

      real(float) :: best_cost = huge(0.0_float) ! cost of the best individual
      integer :: num_cost_evals = 0 ! number of cost evaluations
      integer :: dim = 0 ! dimension of the problem

      type(mt19937) :: rand ! a random number generator

   contains
      ! a finalizer to release the individuals
      final :: finalize_population
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

   subroutine finalize_individual(ind)
      type(Individual), intent(inout) :: ind

      if (associated(ind%genotype)) then
         deallocate(ind%genotype)
         nullify(ind%genotype)
         ! print *, "deallocated genotype"
      end if

   end subroutine finalize_individual

   subroutine finalize_population(pop)
      type(Population), intent(inout) :: pop

      integer :: i

      if (allocated(pop%curr)) then
         do i = 1, pop%pop_size
            call finalize_individual(pop%curr(i))
         end do

         deallocate(pop%curr)

         ! print *, "deallocated pop%curr"
      end if

      if (allocated(pop%best)) then
         do i = 1, pop%pop_size
            call finalize_individual(pop%best(i))
         end do

         deallocate(pop%best)

         ! print *, "deallocated pop%best"
      end if

   end subroutine finalize_population

   ! initialize the population
   subroutine init_population(pop, pop_size, dim, min_val, max_val, seed)
      type(Population), intent(inout) :: pop
      integer, intent(in) :: pop_size, dim
      real(float), intent(in) :: min_val(dim), max_val(dim)
      integer, optional, intent(in) :: seed

      integer :: i

      pop%pop_size = pop_size
      pop%dim = dim

      if (present(seed)) then
         call pop%rand%initialize(seed)
      end if

      allocate(pop%curr(pop_size))
      allocate(pop%best(pop_size))

      do i = 1, pop_size
         allocate(pop%curr(i)%genotype(dim))
         allocate(pop%best(i)%genotype(dim))

         ! initialize the genotype to [min_val, max_val]
         pop%curr(i)%genotype = min_val + (max_val - min_val)*pop%rand%genrand64_real1()
         pop%best(i)%genotype = 0.0_float

         ! print *, "genotype #", i, pop%curr(i)%genotype

         ! cr: [0, 1)
         pop%curr(i)%cr = pop%rand%genrand64_real2()

         ! f: [0.1, 1]
         pop%curr(i)%f = 0.1_float + 0.9_float*pop%rand%genrand64_real1()
      end do

   end subroutine init_population

   subroutine update_pop_best(pop, cost, idx)
      type(Population), intent(inout) :: pop
      real(float), intent(in) :: cost
      integer, intent(in) :: idx

      if (cost .lt. pop%best_cost) then
         pop%best_cost = cost
         pop%best_idx = idx
         ! pop%best(idx)%genotype = pop%curr(idx)%genotype
      end if

   end subroutine update_pop_best

   subroutine update_population(pop, min_val, max_val)
      type(Population), intent(inout) :: pop
      real(float), optional, intent(in) :: min_val(:), max_val(:)

      real(float) :: cost
      integer :: pop_size, dim, forced_mutation_dim
      integer :: idx1, idx2, idx3
      integer :: i, d

      real(float), dimension(:), pointer :: curr_sol, best_sol => null()
      real(float), dimension(:), pointer :: best1_sol, best2_sol, best3_sol => null()

      dim = pop%dim
      pop_size = pop%pop_size

      ! for each individual i sample three distinct individuals (.ne. i)
      do i = 1, pop_size
         ! 'modulo' works with 'int32'
         ! as opposed to 'mod', 'modulo' returns non-negative values

         do
            idx1 = modulo(int(pop%rand%genrand64_int64(), kind=4), pop_size) + 1
            if (idx1 .ne. i) exit
         end do

         do
            idx2 = modulo(int(pop%rand%genrand64_int64(), kind=4), pop_size) + 1
            if (idx2 .ne. idx1) exit
         end do

         do
            idx3 = modulo(int(pop%rand%genrand64_int64(), kind=4), pop_size) + 1
            if (idx3 .ne. idx1 .and. idx3 .ne. idx2) exit
         end do

         if (pop%rand%genrand64_real2() .lt. cr_change_probability) then
            pop%curr(i)%cr = pop%rand%genrand64_real2()
         end if

         if (pop%rand%genrand64_real2() .lt. f_change_probability) then
            pop%curr(i)%f = 0.1_float + 0.9_float*pop%rand%genrand64_real1()
         end if

         curr_sol => pop%curr(i)%genotype
         best_sol => pop%best(i)%genotype

         best1_sol => pop%best(idx1)%genotype
         best2_sol => pop%best(idx2)%genotype
         best3_sol => pop%best(idx3)%genotype

         forced_mutation_dim = modulo(int(pop%rand%genrand64_int64(), kind=4), dim) + 1

         ! This implements the DE/rand/1/bin, the most widely used algorithm.
         ! See "A Comparative Study of Differential Evolution Variants for Global Optimization (2006)".
         do d = 1, dim
            if (pop%rand%genrand64_real2() .lt. pop%curr(i)%cr .or. d .eq. forced_mutation_dim) then
               curr_sol(d) = best1_sol(d) + pop%curr(i)%f*(best2_sol(d) - best3_sol(d))
            else
               curr_sol(d) = best_sol(d)
            end if

            if (present(min_val) .and. present(max_val)) then
               curr_sol(d) = max(min(curr_sol(d), max_val(d)), min_val(d))
            end if
         end do

      end do

   end subroutine update_population

end module differential_evolution
