program tancurve
    implicit none
    real(8), parameter :: pi = 3.141592653
    real(8), dimension(:), allocatable :: pts

    write(*, *) area_under_tan(12)

    contains
    function deg2rad(d) result(r)
        real(8), intent(in) :: d
        real(8) :: r
        r = d * pi / 180.0
    end function deg2rad

    function area_under_tan(pts_num) result(area)
        integer, intent(in) :: pts_num
        real(8), parameter :: a = 0.0
        real(8), parameter :: b = 60.0
        real(8) :: area
        integer :: i

        allocate(pts(0:pts_num))

        do, i = 0, pts_num
            pts(i) = tan(deg2rad(i * (b - a) / pts_num))
        end do

        write(*, "(F10.8)") (pts(i), i = 0, pts_num)

        pts(1:pts_num-1) = pts(1:pts_num-1) * 2.0 
        area = deg2rad(b - a) * sum(pts) / 2.0 / pts_num
        deallocate(pts)
    end function area_under_tan

end program tancurve