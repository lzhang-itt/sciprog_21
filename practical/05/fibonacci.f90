program fibonacci    
    implicit none
    integer :: i, n, input_err
    integer :: fn1, fn2
    
    ! overflow happens very fast with 32 bit values
    write(*, *) "Enter n for fibonacci"
    read(*, *, iostat=input_err) n
    if (input_err /= 0) then
        stop "Ilegal Input"
    end if
    if (n < 0) then
        stop "Ilegal Input"
    end if
    
    select case (n)

    case (0)
        print *, "0, "
    case (1)
        print *, "0, 1, "
    case default
        write(*, "(A)", advance="no") "0, 1, "
        fn1 = 0
        fn2 = 1
        do, i = 2, n
            call fib(fn1, fn2)
            write(*, "(I0,A2)", advance="no") fn2, ", "
        end do
        print *, ""
    end select

    contains
    subroutine fib(p, q)
        ! p is n-2, q is n-1 on enter
        ! p is n-1 while q is n on exit
        integer, intent(inout) :: p, q
        integer :: s
        s = q
        q = p + q
        p = s
    end subroutine
    
end program fibonacci
