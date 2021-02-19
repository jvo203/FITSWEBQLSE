module erloff_error_item_m
    use erloff_error_m, only: error_t
    use erloff_message_type_m, only: message_type_t
    use erloff_module_m, only: module_t
    use erloff_procedure_m, only: procedure_t
    use iso_varying_string, only: varying_string, operator(//)

    implicit none
    private
    public :: error_item_t

    type :: error_item_t
        private
        class(error_t), allocatable :: error
    contains
        private
        procedure, public :: with_names_prepended
        procedure :: is_type
        generic, public :: operator(.isType.) => is_type
        procedure :: originated_from_module
        procedure :: originated_from_procedure
        generic, public :: operator(.originatedFrom.) => &
                originated_from_module, originated_from_procedure
        procedure :: came_through_module
        procedure :: came_through_procedure
        generic, public :: operator(.cameThrough.) => &
                came_through_module, came_through_procedure
        procedure :: is_from_module
        procedure :: is_from_procedure
        generic, public :: operator(.isFrom.) => &
                is_from_module, is_from_procedure
        procedure :: includes_any_of
        generic, public :: operator(.includesAnyOf.) => includes_any_of
        procedure :: includes_all_of
        generic, public :: operator(.includesAllOf.) => includes_all_of
        procedure, public :: to_string
        procedure, public :: repr
    end type

    interface error_item_t
        module procedure constructor
    end interface
contains
    function constructor(error) result(error_item)
        class(error_t), intent(in) :: error
        type(error_item_t) :: error_item

        allocate(error_item%error, source = error)
    end function

    impure elemental function with_names_prepended( &
            self, module_, procedure_) result(new_error)
        class(error_item_t), intent(in) :: self
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(error_item_t) :: new_error

        select type (with_names_prepended => self%error%with_names_prepended(module_, procedure_))
        class is (error_t)
            allocate(new_error%error, source = with_names_prepended)
        end select
    end function

    elemental function is_type(self, type_tag)
        class(error_item_t), intent(in) :: self
        type(message_type_t), intent(in) :: type_tag
        logical :: is_type

        is_type = self%error.isType.type_tag
    end function

    elemental function originated_from_module(self, module_) result(originated_from)
        class(error_item_t), intent(in) :: self
        type(module_t), intent(in) :: module_
        logical :: originated_from

        originated_from = self%error.originatedFrom.module_
    end function

    elemental function originated_from_procedure(self, procedure_) result(originated_from)
        class(error_item_t), intent(in) :: self
        type(procedure_t), intent(in) :: procedure_
        logical :: originated_from

        originated_from = self%error.originatedFrom.procedure_
    end function

    elemental function came_through_module(self, module_) result(came_through)
        class(error_item_t), intent(in) :: self
        type(module_t), intent(in) :: module_
        logical :: came_through

        came_through = self%error.cameThrough.module_
    end function

    elemental function came_through_procedure(self, procedure_) result(came_through)
        class(error_item_t), intent(in) :: self
        type(procedure_t), intent(in) :: procedure_
        logical :: came_through

        came_through = self%error.cameThrough.procedure_
    end function

    elemental function is_from_module(self, module_) result(is_from)
        class(error_item_t), intent(in) :: self
        type(module_t), intent(in) :: module_
        logical :: is_from

        is_from = self%error.isFrom.module_
    end function

    elemental function is_from_procedure(self, procedure_) result(is_from)
        class(error_item_t), intent(in) :: self
        type(procedure_t), intent(in) :: procedure_
        logical :: is_from

        is_from = self%error.isFrom.procedure_
    end function

    pure function includes_any_of(self, strings) result(includes)
        class(error_item_t), intent(in) :: self
        type(varying_string), intent(in) :: strings(:)
        logical :: includes

        includes = self%error.includesAnyOf.strings
    end function

    pure function includes_all_of(self, strings) result(includes)
        class(error_item_t), intent(in) :: self
        type(varying_string), intent(in) :: strings(:)
        logical :: includes

        includes = self%error.includesAllOf.strings
    end function

    elemental function to_string(self) result(string)
        class(error_item_t), intent(in) :: self
        type(varying_string) :: string

        string = self%error%to_string()
    end function

    elemental function repr(self)
        class(error_item_t), intent(in) :: self
        type(varying_string) :: repr

        repr = "error_item_t(error = " // self%error%repr() // ")"
    end function
end module
