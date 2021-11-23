program natural
    integer :: order
    
    order = -1
    print *, "Input order of polynomial"
    read(*, *) order
    if (order < 0) then
        stop "Input a positive integer"
    end if
    print *, estimate(order)

    ! do, order = 1, 100
    !     print *, estimate(order)
    ! end do
    ! I don't see a bigger error as n gets bigger

    contains

    function estimate(n)
        real(8) :: estimate
        real(8) :: factorial
        integer :: i
        factorial = 1.0
        estimate = 1.0
        do i = 1, n
            factorial = factorial*i
            estimate = estimate + (1.0/factorial)
        end do
    end function estimate

end program natural
