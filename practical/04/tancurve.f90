program tancurve
    implicit none
    real(8), parameter :: pi = 3.141592653

    write(*, *) area_under_tan(1200)

    contains
    function deg2rad(d) result(r)
        real(8), intent(in) :: d
        real(8) :: r
        r = d * pi / 180.0
    end function deg2rad

    function area_under_tan(pts_num) result(area)
        integer, intent(in) :: pts_num
        real(8), dimension(0:pts_num) :: pts
        real(8), parameter :: a = 0.0, b = 60.0
        real(8) :: area
        integer :: i
    
        do, i = 0, pts_num
            pts(i) = 2 * tan(deg2rad(i * (b - a) / pts_num))
        end do
        
        pts(0) = pts(0) / 2.0
        pts(pts_num) = pts(pts_num) / 2.0
        
        area = deg2rad(b - a) * sum(pts) / 2.0 / pts_num

    end function area_under_tan

end program tancurve