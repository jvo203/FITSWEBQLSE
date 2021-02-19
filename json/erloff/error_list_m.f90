module erloff_error_list_m
    use erloff_error_m, only: error_t
    use erloff_error_item_m, only: error_item_t
    use erloff_message_type_m, only: message_type_t
    use iso_varying_string, only: &
            varying_string, assignment(=), operator(//), var_str
    use erloff_module_m, only: module_t
    use erloff_procedure_m, only: procedure_t
    use strff, only: hanging_indent, indent, join, NEWLINE

    implicit none
    private
    public :: error_list_t, size

    type :: error_list_t
        private
        type(error_item_t), allocatable :: errors(:)
    contains
        private
        procedure, public :: with_error_appended
        procedure, public :: with_errors_appended
        procedure :: of_type
        generic, public :: operator(.ofType.) => of_type
        procedure :: of_types
        generic, public :: operator(.ofTypes.) => of_types
        procedure :: originating_from_module
        procedure :: originating_from_modules
        procedure :: originating_from_procedure
        procedure :: originating_from_procedures
        generic, public :: operator(.originatingFrom.) => &
                originating_from_module, &
                originating_from_modules, &
                originating_from_procedure, &
                originating_from_procedures
        procedure :: coming_through_module
        procedure :: coming_through_modules
        procedure :: coming_through_procedure
        procedure :: coming_through_procedures
        generic, public :: operator(.comingThrough.) => &
                coming_through_module, &
                coming_through_modules, &
                coming_through_procedure, &
                coming_through_procedures
        procedure :: from_module
        procedure :: from_modules
        procedure :: from_procedure
        procedure :: from_procedures
        generic, public :: operator(.from.) => &
                from_module, &
                from_modules, &
                from_procedure, &
                from_procedures
        procedure :: including_c
        procedure :: including_s
        generic, public :: operator(.including.) => including_c, including_s
        procedure :: including_any_of
        generic, public :: operator(.includingAnyOf.) => including_any_of
        procedure :: including_all_of
        generic, public :: operator(.includingAllOf.) => including_all_of
        procedure, public :: has_any
        procedure :: has_type
        generic, public :: operator(.hasType.) => has_type
        procedure :: has_any_originating_from_module
        procedure :: has_any_originating_from_procedure
        generic, public :: operator(.hasAnyOriginatingFrom.) => &
                has_any_originating_from_module, has_any_originating_from_procedure
        procedure :: has_any_coming_through_module
        procedure :: has_any_coming_through_procedure
        generic, public :: operator(.hasAnyComingThrough.) => &
                has_any_coming_through_module, has_any_coming_through_procedure
        procedure :: has_any_from_module
        procedure :: has_any_from_procedure
        generic, public :: operator(.hasAnyFrom.) => &
                has_any_from_module, has_any_from_procedure
        procedure :: has_any_including_c
        procedure :: has_any_including_s
        generic, public :: operator(.hasAnyIncluding.) => &
                has_any_including_c, has_any_including_s
        procedure :: has_any_including_any_of
        generic, public :: operator(.hasAnyIncludingAnyOf.) => &
                has_any_including_any_of
        procedure :: has_any_including_all_of
        generic, public :: operator(.hasAnyIncludingAllOf.) => &
                has_any_including_all_of
        procedure, public :: to_string
        procedure, public :: repr
    end type

    interface error_list_t
        module procedure new_list
        module procedure from_existing_list
    end interface

    interface size
        module procedure error_list_size
    end interface
contains
    function new_list(error)
        class(error_t), intent(in) :: error
        type(error_list_t) ::  new_list

        allocate(new_list%errors, source = [error_item_t(error)])
    end function

    function from_existing_list(errors, module_, procedure_) result(new_list)
        type(error_list_t), intent(in) :: errors
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(error_list_t) :: new_list

        if (allocated(errors%errors)) then
            allocate(new_list%errors, source = &
                    errors%errors%with_names_prepended(module_, procedure_))
        end if
    end function

    function with_error_appended(self, error) result(new_list)
        class(error_list_t), intent(in) :: self
        class(error_t), intent(in) :: error
        type(error_list_t) :: new_list

        if (allocated(self%errors)) then
            allocate(new_list%errors, source = [self%errors, error_item_t(error)])
        else
            allocate(new_list%errors, source = [error_item_t(error)])
        end if
    end function

    function with_errors_appended( &
            self, errors, module_, procedure_) result(new_list)
        class(error_list_t), intent(in) :: self
        type(error_list_t), intent(in) :: errors
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(error_list_t) :: new_list

        if (allocated(self%errors)) then
            if (allocated(errors%errors)) then
                allocate(new_list%errors, source = &
                        [ self%errors &
                        , errors%errors%with_names_prepended(module_, procedure_) &
                        ])
            else
                allocate(new_list%errors, source = self%errors)
            end if
        else
            if (allocated(errors%errors)) then
                allocate(new_list%errors, source = &
                        [errors%errors%with_names_prepended(module_, procedure_)])
            end if
        end if
    end function

    function of_type(self, type_tag) result(new_list)
        class(error_list_t), intent(in) :: self
        type(message_type_t), intent(in) :: type_tag
        type(error_list_t) :: new_list

        new_list = self.ofTypes.[type_tag]
    end function

    function of_types(self, type_tags) result(new_list)
        class(error_list_t), intent(in) :: self
        type(message_type_t), intent(in) :: type_tags(:)
        type(error_list_t) :: new_list

        integer :: i

        if (allocated(self%errors)) then
            allocate(new_list%errors, source = pack( &
                    self%errors, &
                    mask = [(any(self%errors(i).isType.type_tags), i = 1, size(self))]))
        end if
    end function

    function originating_from_module(self, module_) result(new_list)
        class(error_list_t), intent(in) :: self
        type(module_t), intent(in) :: module_
        type(error_list_t) :: new_list

        new_list = self.originatingFrom.[module_]
    end function

    function originating_from_modules(self, modules) result(new_list)
        class(error_list_t), intent(in) :: self
        type(module_t), intent(in) :: modules(:)
        type(error_list_t) :: new_list

        integer :: i

        if (allocated(self%errors)) then
            allocate(new_list%errors, source = pack( &
                    self%errors, &
                    mask = [(any(self%errors(i).originatedFrom.modules), i = 1, size(self))]))
        end if
    end function

    function originating_from_procedure(self, procedure_) result(new_list)
        class(error_list_t), intent(in) :: self
        type(procedure_t), intent(in) :: procedure_
        type(error_list_t) :: new_list

        new_list = self.originatingFrom.[procedure_]
    end function

    function originating_from_procedures(self, procedures) result(new_list)
        class(error_list_t), intent(in) :: self
        type(procedure_t), intent(in) :: procedures(:)
        type(error_list_t) :: new_list

        integer :: i

        if (allocated(self%errors)) then
            allocate(new_list%errors, source = pack( &
                    self%errors, &
                    mask = [(any(self%errors(i).originatedFrom.procedures), i = 1, size(self))]))
        end if
    end function

    function coming_through_module(self, module_) result(new_list)
        class(error_list_t), intent(in) :: self
        type(module_t), intent(in) :: module_
        type(error_list_t) :: new_list

        new_list = self.comingThrough.[module_]
    end function

    function coming_through_modules(self, modules) result(new_list)
        class(error_list_t), intent(in) :: self
        type(module_t), intent(in) :: modules(:)
        type(error_list_t) :: new_list

        integer :: i

        if (allocated(self%errors)) then
            allocate(new_list%errors, source = pack( &
                    self%errors, &
                    mask = [(any(self%errors(i).cameThrough.modules), i = 1, size(self))]))
        end if
    end function

    function coming_through_procedure(self, procedure_) result(new_list)
        class(error_list_t), intent(in) :: self
        type(procedure_t), intent(in) :: procedure_
        type(error_list_t) :: new_list

        new_list = self.comingThrough.[procedure_]
    end function

    function coming_through_procedures(self, procedures) result(new_list)
        class(error_list_t), intent(in) :: self
        type(procedure_t), intent(in) :: procedures(:)
        type(error_list_t) :: new_list

        integer :: i

        if (allocated(self%errors)) then
            allocate(new_list%errors, source = pack( &
                    self%errors, &
                    mask = [(any(self%errors(i).cameThrough.procedures), i = 1, size(self))]))
        end if
    end function

    function from_module(self, module_) result(new_list)
        class(error_list_t), intent(in) :: self
        type(module_t), intent(in) :: module_
        type(error_list_t) :: new_list

        new_list = self.from.[module_]
    end function

    function from_modules(self, modules) result(new_list)
        class(error_list_t), intent(in) :: self
        type(module_t), intent(in) :: modules(:)
        type(error_list_t) :: new_list

        integer :: i

        if (allocated(self%errors)) then
            allocate(new_list%errors, source = pack( &
                    self%errors, &
                    mask = [(any(self%errors(i).isFrom.modules), i = 1, size(self))]))
        end if
    end function

    function from_procedure(self, procedure_) result(new_list)
        class(error_list_t), intent(in) :: self
        type(procedure_t), intent(in) :: procedure_
        type(error_list_t) :: new_list

        new_list = self.from.[procedure_]
    end function

    function from_procedures(self, procedures) result(new_list)
        class(error_list_t), intent(in) :: self
        type(procedure_t), intent(in) :: procedures(:)
        type(error_list_t) :: new_list

        integer :: i

        if (allocated(self%errors)) then
            allocate(new_list%errors, source = pack( &
                    self%errors, &
                    mask = [(any(self%errors(i).isFrom.procedures), i = 1, size(self))]))
        end if
    end function

    function including_c(self, string) result(new_list)
        class(error_list_t), intent(in) :: self
        character(len=*), intent(in) :: string
        type(error_list_t) :: new_list

        new_list = self.including.var_str(string)
    end function

    function including_s(self, string) result(new_list)
        class(error_list_t), intent(in) :: self
        type(varying_string), intent(in) :: string
        type(error_list_t) :: new_list

        new_list = self.includingAnyOf.[string]
    end function

    function including_any_of(self, strings) result(new_list)
        class(error_list_t), intent(in) :: self
        type(varying_string), intent(in) :: strings(:)
        type(error_list_t) :: new_list

        integer :: i

        if (allocated(self%errors)) then
            allocate(new_list%errors, source = pack( &
                    self%errors, &
                    mask = [(self%errors(i).includesAnyOf.strings, i = 1, size(self))]))
        end if
    end function

    function including_all_of(self, strings) result(new_list)
        class(error_list_t), intent(in) :: self
        type(varying_string), intent(in) :: strings(:)
        type(error_list_t) :: new_list

        integer :: i

        if (allocated(self%errors)) then
            allocate(new_list%errors, source = pack( &
                    self%errors, &
                    mask = [(self%errors(i).includesAllOf.strings, i = 1, size(self))]))
        end if
    end function

    pure function has_any(self)
        class(error_list_t), intent(in) :: self
        logical :: has_any

        has_any = size(self) > 0
    end function

    function has_type(self, type_tag)
        class(error_list_t), intent(in) :: self
        type(message_type_t), intent(in) :: type_tag
        logical :: has_type

        has_type = size(self.ofType.type_tag) > 0
    end function

    function has_any_originating_from_module(self, module_) result(has_any)
        class(error_list_t), intent(in) :: self
        type(module_t), intent(in) :: module_
        logical :: has_any

        has_any = size(self.originatingFrom.module_) > 0
    end function

    function has_any_originating_from_procedure(self, procedure_) result(has_any)
        class(error_list_t), intent(in) :: self
        type(procedure_t), intent(in) :: procedure_
        logical :: has_any

        has_any = size(self.originatingFrom.procedure_) > 0
    end function

    function has_any_coming_through_module(self, module_) result(has_any)
        class(error_list_t), intent(in) :: self
        type(module_t), intent(in) :: module_
        logical :: has_any

        has_any = size(self.comingThrough.module_) > 0
    end function

    function has_any_coming_through_procedure(self, procedure_) result(has_any)
        class(error_list_t), intent(in) :: self
        type(procedure_t), intent(in) :: procedure_
        logical :: has_any

        has_any = size(self.comingThrough.procedure_) > 0
    end function

    function has_any_from_module(self, module_) result(has_any)
        class(error_list_t), intent(in) :: self
        type(module_t), intent(in) :: module_
        logical :: has_any

        has_any = size(self.from.module_) > 0
    end function

    function has_any_from_procedure(self, procedure_) result(has_any)
        class(error_list_t), intent(in) :: self
        type(procedure_t), intent(in) :: procedure_
        logical :: has_any

        has_any = size(self.from.procedure_) > 0
    end function

    function has_any_including_c(self, string) result(has_any)
        class(error_list_t), intent(in) :: self
        character(len=*), intent(in) :: string
        logical :: has_any

        has_any = size(self.including.string) > 0
    end function

    function has_any_including_s(self, string) result(has_any)
        class(error_list_t), intent(in) :: self
        type(varying_string), intent(in) :: string
        logical :: has_any

        has_any = size(self.including.string) > 0
    end function

    function has_any_including_any_of(self, strings) result(has_any)
        class(error_list_t), intent(in) :: self
        type(varying_string), intent(in) :: strings(:)
        logical :: has_any

        has_any = size(self.includingAnyOf.strings) > 0
    end function

    function has_any_including_all_of(self, strings) result(has_any)
        class(error_list_t), intent(in) :: self
        type(varying_string), intent(in) :: strings(:)
        logical :: has_any

        has_any = size(self.includingAllOf.strings) > 0
    end function

    pure function to_string(self) result(string)
        class(error_list_t), intent(in) :: self
        type(varying_string) :: string

        if (allocated(self%errors)) then
            string = join(self%errors%to_string(), NEWLINE)
        else
            string = ""
        end if
    end function

    pure function repr(self)
        class(error_list_t), intent(in) :: self
        type(varying_string) :: repr

        type(varying_string) :: errors_string

        if (allocated(self%errors)) then
            errors_string = "[" // NEWLINE &
                    // indent( &
                            join(self%errors%repr(), "," // NEWLINE), &
                            4) // NEWLINE // "]"
        else
            errors_string = "UNALLOCATED"
        end if

        repr = hanging_indent( &
                "error_list_t(" // NEWLINE &
                    // "errors = " // errors_string, &
                4) // NEWLINE // ")"
    end function

    pure function error_list_size(self) result(length)
        class(error_list_t), intent(in) :: self
        integer :: length

        if (allocated(self%errors)) then
            length = size(self%errors)
        else
            length = 0
        end if
    end function
end module
