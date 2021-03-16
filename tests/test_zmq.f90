program main
    use, intrinsic :: iso_c_binding
    use :: omp_lib
    use :: zmq
    implicit none
    integer, target :: value
    integer         :: rc
    type(c_ptr)     :: context
    type(c_ptr)     :: socket

    print '(a)', 'ZeroMQ Test'
    print '(a)', repeat('-', 32)

    ! Set OpenMP options.
    call omp_set_dynamic(.false.)
    call omp_set_num_threads(2)

    print '(a)', 'Creating new context ...'
    context = zmq_ctx_new()

    !$omp parallel shared(context), private(socket, rc, value)
    !$omp sections
    !$omp section

    ! Sender thread.
    print '("[Thread ", i0, "] Thread created")', omp_get_thread_num()
    socket = zmq_socket(context, ZMQ_PAIR)
    rc = zmq_connect(socket, 'inproc://fzmq')

    value = 123
    print '("[Thread ", i0, "] Sending value ", i0, " ...")', omp_get_thread_num(), value
    call send(socket, value)
    rc = zmq_close(socket)

    !$omp section

    ! Receiver thread.
    print '("[Thread ", i0, "] Thread created")', omp_get_thread_num()
    socket = zmq_socket(context, ZMQ_PAIR)
    rc = zmq_bind(socket, 'inproc://fzmq')

    call recv(socket, value)
    print '("[Thread ", i0, "] Received value ", i0)', omp_get_thread_num(), value
    rc = zmq_close(socket)

    !$omp end sections
    !$omp end parallel

    print '(a)', 'Terminating context ...'
    rc = zmq_ctx_term(context)
contains
    subroutine send(socket, value)
        type(c_ptr), intent(inout) :: socket
        integer(kind=c_int), pointer, intent(in)    :: value
        integer(kind=c_int)                         :: nbytes
        integer(kind=c_int)                         :: rc
        type(zmq_msg_t)                             :: message

        INTEGER(KIND=C_SIZE_T) :: msg_len

        msg_len = sizeof(value)

        rc = zmq_msg_init_data(message, c_loc(value), msg_len, c_null_funptr, c_null_ptr)
        nbytes = zmq_msg_send(message, socket, 0)
    end subroutine

    subroutine recv(socket, value)
        type(c_ptr), intent(inout)             :: socket
        integer, intent(out)               :: value
        character(kind=c_char, len=:), pointer :: buffer
        character(kind=c_char, len=:), pointer :: range
        integer                                :: nbytes
        integer                                :: rc
        type(c_ptr)                            :: data
        type(zmq_msg_t)                        :: message

        rc = zmq_msg_init(message)
        nbytes = zmq_msg_recv(message, socket, 0)
        data = zmq_msg_data(message)

        call c_f_pointer(data, buffer)
        range => buffer(1:c_sizeof(value))
        value = transfer(range, value)

        rc = zmq_msg_close(message)
    end subroutine
end program main
