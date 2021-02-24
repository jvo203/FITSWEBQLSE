program test_serialize

    use json_module

    implicit none

    type(json_file) :: json
    logical :: found
    integer :: i, j, k

    CHARACTER(kind=json_CK, len=:), allocatable :: str_val

    ! initialize the class
    call json%initialize()

    ! read the file
    call json%load(filename='Titan_HC3N_4039.pbcor.json')

    ! print the file to the console
    ! call json%print()

    print *, '[to_json]::16'

    ! serialize to string prior to further handling
    call json%serialize(str_val)

    print *, '[to_json]::17'

    print *, str_val

    ! clean up
    call json%destroy()
    if (json%failed()) stop 1

end program test_serialize
