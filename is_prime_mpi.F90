module subs
  use mpi_f08
  implicit none
  integer :: iam, np
  integer(8) :: istart, iend
  integer(8) :: size_l, size_mod
#ifdef _DEBUG
  integer(8),allocatable,dimension(:) :: array ! for debug
#endif
  integer(8),allocatable,dimension(:) :: array_l, sizes
  
contains
  subroutine myinit(size)
    implicit none
    integer(8),intent(inout) :: size
    integer(8) :: i
    character(len=32) :: argv1

    call mpi_init
    call mpi_comm_rank(mpi_comm_world, iam)
    call mpi_comm_size(mpi_comm_world, np)

    if (command_argument_count() == 0) then
       size = 7
    else
       call get_command_argument(1, argv1)
       read(argv1, *) size
    end if

#ifdef _DEBUG
    if (iam == 0) then
       allocate(array(size), sizes(np))
       do i = 1, size
          array(i) = i
       end do
       write(6, *) "size:", size
    end if
    call mpi_barrier(mpi_comm_world)
#endif

    size_mod = mod(size, np)
    size_l = size/np
    if (iam < size_mod) then
       size_l = size_l + 1
    end if

    if (iam < size_mod) then
       istart = iam*size_l + 1
    else
       istart = iam*size_l + 1 + size_mod
    end if
    iend   = istart + size_l - 1

    allocate(array_l(istart:iend))
    do i = istart, iend
       array_l(i) = i
    end do
#ifdef _DEBUG
    write(6, '(a, 3i4)') "iam, istart, iend:", iam, istart, iend
#endif
  end subroutine myinit

  subroutine is_prime(val, is_prim)
    use mpi_f08
    !$use omp_lib
    implicit none
    integer(8),intent(in) :: val
    logical,intent(out) :: is_prim
    integer(8) :: i
    !$ logical :: is_prim_p
   
    is_prim = .true.

    if (val == 2) then
       is_prim = .true.
       return
    end if

    if (val == 1 .or. mod(val, 2) == 0) then
       is_prim = .false.
       return
    end if

    if (iam == 0) then
       !$omp parallel private(i, is_prim_p)
       !$ is_prim_p = .true.
       !$omp do reduction(*:is_prim)
       do i = 3, val-1, 2
          if (mod(val, i) == 0) then
#ifdef _OPENMP
             !$ is_prim_p = .false.
#else
             is_prim = .false.
             return
#endif
          end if
#ifdef _OPENMP
          !$ is_prim = is_prim * is_prim_p ! assuming .false. is zero...
#endif
       end do
       !$omp end do
       !$omp end parallel
#ifndef _OPENMP
       is_prim = .true.
#endif
    else
       !$omp parallel private(i, is_prim_p)
       !$ is_prim_p = .true.
       !$omp do reduction(*:is_prim)
       do i = istart, iend-1, 2
          if (mod(val, i) == 0) then
#ifdef _OPENMP
             !$ is_prim_p = .false.
#else
             is_prim = .false.
             return
#endif
          end if
#ifdef _OPENMP
          !$ is_prim = is_prim * is_prim_p ! assuming .false. is zero...
#endif
       end do
       !$omp end do
       !$omp end parallel
#ifndef _OPENMP
       is_prim = .true.
#endif 
    end if

  end subroutine is_prime
  
  subroutine myfini
    implicit none

#ifdef _DEBUG
    if (iam == 0) deallocate(array)
#endif
    deallocate(array_l)
    call mpi_finalize
  end subroutine myfini

end module subs

program main
  use mpi_f08
  use subs
  implicit none
  integer(8) :: val
  character(len=32) :: argv1
  logical :: is_prim

  call myinit(val)
  
  if (iam == 0) write(6, *) "val:", val
  
  call is_prime(val, is_prim)
  
  if (iam == 0) then
     if (is_prim) then
        write(6, *) "this is a prime number."
     else
        write(6, *) "this is not a prime number"
     end if
  endif

  call myfini
  stop
end program main
