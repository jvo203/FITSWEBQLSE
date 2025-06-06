! A generic linked list object, downloaded from the Internet
! Lightly corrected by Christopher Zapart

module list
   implicit none

   private

   public :: list_t
   public :: list_init
   public :: list_free
   public :: list_insert
   public :: list_put
   public :: list_get
   public :: list_next

   ! Linked list node data type
   type :: list_t
      private
      integer, dimension(:), pointer :: data => null()
      type(list_t), pointer :: next => null()
   end type list_t

contains

   ! Initialize a head node SELF and optionally store the provided DATA.
   subroutine list_init(self, data)
      type(list_t), pointer :: self
      integer, dimension(:), intent(in), optional :: data

      allocate (self)
      nullify (self%next)

      if (present(data)) then
         allocate (self%data(size(data)))
         self%data = data
      else
         nullify (self%data)
      end if
   end subroutine list_init

   ! Free the entire list and all data, beginning at SELF
   subroutine list_free(self)
      type(list_t), pointer :: self
      type(list_t), pointer :: current
      type(list_t), pointer :: next

      current => self
      do while (associated(current))
         next => current%next
         if (associated(current%data)) then
            deallocate (current%data)
            nullify (current%data)
         end if
         deallocate (current)
         nullify (current)
         current => next
      end do
   end subroutine list_free

   ! Return the next node after SELF
   function list_next(self) result(next)
      type(list_t), pointer :: self
      type(list_t), pointer :: next
      next => self%next
   end function list_next

   ! Insert a new node at a front of a list node SELF containing DATA (optional)
   subroutine list_insert(self, data)
      type(list_t), intent(inout), pointer :: self
      integer, dimension(:), intent(in), optional :: data
      type(list_t), pointer :: next

      allocate (next)

      if (present(data)) then
         allocate (next%data(size(data)))
         next%data = data
      else
         nullify (next%data)
      end if

      ! point a new node to the current head node
      next%next => self

      ! re-point the head of the list
      self => next
   end subroutine list_insert

   ! Store the encoded DATA in list node SELF
   subroutine list_put(self, data)
      type(list_t), pointer :: self
      integer, dimension(:), intent(in) :: data

      if (associated(self%data)) then
         deallocate (self%data)
         nullify (self%data)
      end if
      self%data = data
   end subroutine list_put

   ! Return the DATA stored in the node SELF
   function list_get(self) result(data)
      type(list_t), pointer :: self
      integer, dimension(:), pointer :: data
      data => self%data
   end function list_get

end module list
