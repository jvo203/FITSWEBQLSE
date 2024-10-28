program main
    use differential_evolution
    use omp_lib
    implicit none

    ! make dim a parameter
    integer, parameter :: dim = 2

    integer :: seed = 123
    integer :: pop_size = 20
    real(float) :: min_val(dim), max_val(dim)
    type(Population) :: pop

    integer :: i

    ! set the min and max values for each dimension to [-10, 10]
    min_val(:) = -10.0
    max_val(:) = 10.0

    ! initialize the population
    call init_population(pop, pop_size, dim, seed, min_val, max_val)

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
    
end program main