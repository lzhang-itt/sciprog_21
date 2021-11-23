program hyperbolic
    implicit none
    real(8) :: delta, diff
    integer :: read_status
    real(8), dimension(:), allocatable :: macresults, logresults, xvals
    real(8), parameter :: upper = 0.99
    real(8), parameter :: lower = -0.99
    real(8), parameter :: step = 0.01
    integer:: arrlen, i

    print *, "Enter a small positive number."
    read(*, *, iostat=read_status) delta
    if (read_status /= 0) then
        stop "Ilegal input"
    end if
    if (delta <= 0.0) then
        stop "Positive number required"
    end if

    arrlen = ceiling((upper - lower) / step) - 1
    allocate(macresults(0:arrlen))
    allocate(logresults(0:arrlen))
    allocate(xvals(0:arrlen))
    write(*, *) "x      Maclaurin       Naturallog      Difference"
    do, i = 0, arrlen
        xvals(i) = lower + step * i
        macresults(i) = artanh1(xvals(i))
        logresults(i) = artanh2(xvals(i))
        diff = macresults(i)-logresults(i)
        write(*, "(F4.2,A2,F14.10,A2,F14.10,A2,F14.10)") xvals(i), ", ", macresults(i), ", ", logresults(i), ", ", diff
    end do
    ! A sufficiently small delta (0.000000001) can actually lower error to acceptable level. 
    ! The impact is close to linear.

    contains
    function artanh1(x) result(ret)
        real(8), intent(in) :: x
        real(8) :: val, newval
        integer(8) :: n = 0
        real(8) :: ret
        
        val = 0
        newval = x
        n = 1
        do while (abs(newval) > delta)
            val = val + newval
            newval = ( (x**((2*n)+1)) / ((2*n)+1) )
            n = n + 1
        end do
        ret = val
    end function artanh1

    function artanh2(x) result(ret)
        real(8), intent(in) :: x
        real(8) :: ret

        ret = (log(1.0 + x) - log(1.0 - x)) / 2.0
    end function artanh2
        
end program hyperbolic