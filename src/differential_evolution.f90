module differential_evolution
   use,intrinsic :: iso_fortran_env
   use mt19937_64
   use omp_lib

   implicit none

   public

   integer,parameter :: float = real32  ! single precision real kind [4 bytes].
   ! integer,parameter :: float = real64  ! double precision real kind [8 bytes].

   type Individual
      real(float), dimension(:), allocatable :: genotype ! the genotype of the individual solution
      real(float) :: cost = huge(0.0_float) ! the cost of the individual solution

      ! control parameters
      real(float) :: cr = 0.0_float
      real(float) :: f = 0.0_float
   end type Individual

   type :: Population
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

   ! initialize the population
   subroutine init_population(pop, pop_size, dim, seed, min_val, max_val)
      type(Population), intent(inout) :: pop
      integer, intent(in) :: pop_size, dim
      integer, intent(in) :: seed
      real(float), intent(in) :: min_val(dim), max_val(dim)

      integer :: i

      pop%pop_size = pop_size
      pop%dim = dim

      call pop%rand%initialize(seed)

      allocate(pop%curr(pop_size))
      allocate(pop%best(pop_size))

      do i = 1, pop_size
         allocate(pop%curr(i)%genotype(dim))
         allocate(pop%best(i)%genotype(dim))

         ! initialize the genotype to [min_val, max_val]
         pop%curr(i)%genotype = min_val + (max_val - min_val)*pop%rand%genrand64_real1()
         pop%best(i)%genotype = 0.0_float

         print *, "genotype #", i, pop%curr(i)%genotype

         ! cr: [0, 1)
         pop%curr(i)%cr = pop%rand%genrand64_real2()

         ! f: [0.1, 1]
         pop%curr(i)%f = 0.1_float + 0.9_float*pop%rand%genrand64_real1()
      end do

   end subroutine init_population

   subroutine update_population(pop)
      type(Population), intent(inout) :: pop

      real(float) :: cost
      integer(int32) :: pop_size, dim, i
      integer(int32) :: idx1, idx2, idx3

      pop_size = pop%pop_size

      do i = 1, pop_size
         ! sample three different individuals
         idx1 = int(pop%rand%genrand64_int64(), int32) ! % pop_size + 1

         do
            idx2 = int(pop%rand%genrand64_int64(), int32) ! % pop_size + 1
            if (idx2 .ne. idx1) exit
         end do

         do
            idx3 = int(pop%rand%genrand64_int64(), int32) ! % pop_size + 1
            if (idx3 .ne. idx1 .and. idx3 .ne. idx2) exit
         end do

         print *, i, idx1, idx2, idx3
      end do

   end subroutine update_population

end module differential_evolution
