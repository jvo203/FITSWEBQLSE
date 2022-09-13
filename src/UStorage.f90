module ustorage_m
    
    use iso_c_binding, only: c_int8_t, c_loc, c_f_pointer, c_ptr
    
    implicit none
    
    private
    
    type, public :: ustorage_t(len)
        integer, len :: len
        integer(c_int8_t), dimension(len) :: data
    contains
        procedure, pass(self) :: store => ustorage_store
        procedure, pass(self) :: retrieve => ustorage_retrieve
    end type
    
contains
    
    subroutine ustorage_get_pointer(item, n, pitem)
        type(*), intent(in), target :: item
        integer, intent(in) :: n
        integer(c_int8_t), dimension(:), pointer, intent(inout) :: pitem
        type(c_ptr) :: cp
        cp = c_loc(item)
        call c_f_pointer(cp, pitem, [n])
    end subroutine
    
    subroutine ustorage_store(self, item)
        class(ustorage_t(*)), intent(inout) :: self
        type(*), intent(in) :: item
        integer(c_int8_t), dimension(:), pointer :: pitem        
        call ustorage_get_pointer(item, self%len, pitem)
        self%data(:) = pitem(:)
    end subroutine
    
    subroutine ustorage_retrieve(self, item)
        class(ustorage_t(*)), intent(in) :: self
        type(*), intent(inout) :: item
        integer(c_int8_t), dimension(:), pointer :: pitem        
        call ustorage_get_pointer(item, self%len, pitem)
        pitem(:) = self%data(:)        
    end subroutine
    
end module