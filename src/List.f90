module list_m
    
    use ustorage_m
    
    implicit none
    
    private
    
    type, public :: list_t(len)
        integer, len :: len
        integer :: ilast
        type(ustorage_t(len)), dimension(:), allocatable :: items
    contains
        procedure, pass(self) :: adjust_size => list_adjust_size
        procedure, pass(self) :: adjust_size_to => list_adjust_size_to
        procedure, pass(self) :: push => list_push
        procedure, pass(self) :: set => list_set
        procedure, pass(self) :: get => list_get
        procedure, pass(self) :: pop => list_pop
        procedure, pass(self) :: capacity => list_capacity
        procedure, pass(self) :: size => list_size
        procedure, pass(self) :: init => list_init
    end type
    
contains
    
    subroutine list_init(self, init_capacity)
        class(list_t(*)), intent(inout) :: self
        integer, intent(in) :: init_capacity
        type(ustorage_t(self%len)) :: dummy_mold
        integer :: i
        allocate(self%items(init_capacity), source = dummy_mold)
        self%ilast = 0
    end subroutine
    
    subroutine list_adjust_size(self)
        class(list_t(*)), intent(inout) :: self
        
        type(ustorage_t(self%len)), dimension(:), allocatable :: temp
        integer :: nitems
        
        nitems = size(self%items)
        
        if (self%ilast == nitems) then
            call move_alloc(self%items, temp)
            allocate(self%items(nitems * 2))
            self%items(1:self%ilast) = temp
            deallocate(temp)
        end if
        
    end subroutine
    
    
    subroutine list_adjust_size_to(self, n)
        class(list_t(*)), intent(inout) :: self
        integer, intent(in) :: n
        type(ustorage_t(self%len)), dimension(:), allocatable :: temp
        integer :: nitems
        
        nitems = size(self%items)
        
        if (n > nitems) then
            call move_alloc(self%items, temp)
            allocate(self%items(n))
            self%items(1:self%ilast) = temp
            deallocate(temp)
        end if
        
    end subroutine
    
    
    subroutine list_push(self, item)
        class(list_t(*)), intent(inout) :: self
        type(*), intent(in), target :: item
        call self%adjust_size()
        self%ilast = self%ilast + 1
        call self%items(self%ilast)%store(item)
    end subroutine
    
    subroutine list_set(self, i, item)
        class(list_t(*)), intent(inout) :: self
        integer, intent(in) :: i
        type(*), intent(in), target :: item
        call self%adjust_size_to(i)
        ! note: items between (old) ilast and i become undefined
        if (i > self%ilast) then
            self%ilast = i
        end if
        call self%items(self%ilast)%store(item)
    end subroutine
    
    subroutine list_get(self, i, item)
        class(list_t(*)), intent(in) :: self
        integer, intent(in) :: i
        type(*), target :: item
        call self%items(i)%retrieve(item)
    end subroutine
    
    subroutine list_pop(self, item)
        class(list_t(*)), intent(inout) :: self
        type(*), intent(inout), target :: item
        call self%get(self%ilast, item)
        self%ilast = self%ilast - 1
    end subroutine
    
    pure integer function list_capacity(self) result(n)
        class(list_t(*)), intent(in) :: self
        n = size(self%items)
    end function
    
    pure integer function list_size(self) result(n)
        class(list_t(*)), intent(in) :: self
        n = self%ilast
    end function
    

end module
        