program trapezodial
    implicit none
    real(8), parameter :: pi = 3.141592653
    integer, parameter :: pts_num = 12
    real(8), dimension(0:pts_num) :: pts
    real(8) :: a, b, sumab, logresult, sumtrape
    integer :: i
    
    a = 0
    b = pi / 3.0

    sumab = tan(a) + tan(b)
    write (*, *) "Sum of tan(a) tab(b)"
    write (*, *) sumab

    do, i = 0, pts_num
        pts(i) = 2 * tan(i * (b - a) / pts_num)
    end do
    
    pts(0) = pts(0) / 2.0
    pts(pts_num) = pts(pts_num) / 2.0
    
    sumtrape = (b - a) * sum(pts) / 2.0 / pts_num
    logresult = log(2.0)

    write (*, *) "Area by approximation"
    write (*, *) sumtrape
    write (*, *) "Area exact result"
    write (*, *) logresult
    write (*, *) "Difference"
    write (*, *) sumtrape - logresult

    ! That's a bit off but reasonable.
    ! 12 points 1.8e-3
    ! 1209 points 2.4e-7
    ! 120000 points 4.6e-8

end program trapezodial