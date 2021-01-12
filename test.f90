!
! Copyright 2003-2020 Intel Corporation.
! 
! This software and the related documents are Intel copyrighted materials, and
! your use of them is governed by the express license under which they were
! provided to you (License). Unless the License provides otherwise, you may
! not use, modify, copy, publish, distribute, disclose or transmit this
! software or the related documents without Intel's prior written permission.
! 
! This software and the related documents are provided as is, with no express
! or implied warranties, other than those that are expressly stated in the
! License.
!
        program main
        use mpi
        implicit none

        integer i, size, rank, namelen, ierr
        character (len=MPI_MAX_PROCESSOR_NAME) :: name
        integer stat(MPI_STATUS_SIZE)

        call MPI_INIT (ierr)

        call MPI_COMM_SIZE (MPI_COMM_WORLD, size, ierr)
        call MPI_COMM_RANK (MPI_COMM_WORLD, rank, ierr)
        call MPI_GET_PROCESSOR_NAME (name, namelen, ierr)

        if (rank.eq.0) then

            print *, 'Hello world: rank ', rank, ' of ', size, ' running on ', name

            do i = 1, size - 1
                call MPI_RECV (rank, 1, MPI_INTEGER, i, 1, MPI_COMM_WORLD, stat, ierr)
                call MPI_RECV (size, 1, MPI_INTEGER, i, 1, MPI_COMM_WORLD, stat, ierr)
                call MPI_RECV (namelen, 1, MPI_INTEGER, i, 1, MPI_COMM_WORLD, stat, ierr)
                name = ''
                call MPI_RECV (name, namelen, MPI_CHARACTER, i, 1, MPI_COMM_WORLD, stat, ierr)
                print *, 'Hello world: rank ', rank, ' of ', size, ' running on ', name
            enddo

        else

            call MPI_SEND (rank, 1, MPI_INTEGER, 0, 1, MPI_COMM_WORLD, ierr)
            call MPI_SEND (size, 1, MPI_INTEGER, 0, 1, MPI_COMM_WORLD, ierr)
            call MPI_SEND (namelen, 1, MPI_INTEGER, 0, 1, MPI_COMM_WORLD, ierr)
            call MPI_SEND (name, namelen, MPI_CHARACTER, 0, 1, MPI_COMM_WORLD, ierr)

        endif

        call MPI_FINALIZE (ierr)

        end
