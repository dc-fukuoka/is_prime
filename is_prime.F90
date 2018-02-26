module subs
  implicit none
contains
  subroutine is_prime(val, is_prim)
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

  end subroutine is_prime
end module subs

program main
  use subs
  implicit none
  integer(8) :: val
  character(len=32) :: argv1
  logical :: is_prim

  if (command_argument_count() == 0) then
     val = 111111
  else
     call get_command_argument(1, argv1)
     read(argv1, *) val
  end if
  write(6, *) "val:", val
  call is_prime(val, is_prim)
  if (is_prim) then
     write(6, *) "this is a prime number."
  else
     write(6, *) "this is not a prime number"
  end if

  stop
end program main
