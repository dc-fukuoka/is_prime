module subs
  use mpi_f08
  implicit none
  integer :: iam, np
  integer(8) :: istart, iend
  
contains
  subroutine myinit(size)
    implicit none
    integer(8),intent(inout) :: size
    integer(8) :: i, size_l, size_mod
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

    if (np > size) then
       if (iam == 0) write(6, *) "np must be smaller than the input value:", size
       call mpi_finalize
       stop
    end if

    size_mod = mod(size, np)
    size_l = size/np
    if (iam < size_mod) then
       size_l = size_l + 1
    end if

    if (iam == 0) then
       istart = 1
    else if (iam > 0 .and. iam < size_mod) then
       istart = iam*size_l + 1
    else
       istart = iam*size_l + size_mod + 1
    end if
    iend   = istart + size_l - 1

#ifdef _DEBUG
    if (iam == 0) write(6, *) "size_mod:", size_mod
    write(6, '(a, 4i4)') "iam, size_l, istart, iend:", iam, size_l, istart, iend
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

    if (val == 2) return

    if (val == 1 .or. mod(val, 2) == 0) then
       is_prim = .false.
       return
    end if

    if (istart == iend .and. iend /= val .and. (istart /= 1 .and. istart /= 2)) then
       i = iend
       if (mod(val, i) == 0) then
          is_prim = .false.
          return
       end if
    end if

    if (iend <= 3) return
    
    if (mod(istart, 2) == 0) istart = istart + 1   
       
    if (iend /= val) then
       do i = istart, iend, 2
          if (i == 1 .or. i == 2) cycle
          if (mod(val, i) == 0) then
             is_prim = .false.
             return
          end if
       end do
    else
       do i = istart, iend-1, 2
          if (i == 1 .or. i == 2) cycle
          if (mod(val, i) == 0) then
             is_prim = .false.
             return
          end if
       end do
    end if

  end subroutine is_prime

  subroutine reduce_land(x)
    implicit none
    logical, intent(inout) :: x
    
    if (iam == 0) then
       call mpi_reduce(mpi_in_place, x, 1, mpi_logical, mpi_land, 0, mpi_comm_world)
    else
       call mpi_reduce(x,            x, 1, mpi_logical, mpi_land, 0, mpi_comm_world)
    end if
  end subroutine reduce_land
  
  subroutine myfini
    implicit none

    call mpi_finalize
  end subroutine myfini

end module subs

program main
  use subs
  implicit none
  integer(8) :: val
  logical :: is_prim

  call myinit(val)

  if (iam == 0) write(6, *) "val:", val
  
  call is_prime(val, is_prim)
#ifdef _DEBUG
  write(6, *) "debug: is_prim:", is_prim
#endif
  call reduce_land(is_prim)
  
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
