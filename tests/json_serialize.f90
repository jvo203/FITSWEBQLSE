program test_serialize

    use json_module

    implicit none

    type(json_file) :: json
    logical :: found
    integer :: i, j, k

    ! initialize the class
    call json%initialize()

    ! read the file
    call json%load(filename='Titan_HC3N_4039.pbcor.json')

    ! print the file to the console
    call json%print()

    ! clean up
    call json%destroy()
    if (json%failed()) stop 1

end program test_serialize
