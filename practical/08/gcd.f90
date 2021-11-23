program commondivisor
    implicit none
    integer, parameter :: l = 8
    integer, dimension(l), parameter :: a_list = (/4, 8, 5, 19, 42, 36, 20, 64/)
    integer, dimension(l), parameter :: b_list = (/1, 2, 5, 38, 7, 82, 194, 932/)
    integer, dimension(l) :: g_list
    integer :: i

    do, i = 1, l
        g_list(i) = gcd(a_list(i), b_list(i))
        write(*, "(I4, I4, A4, I4)") a_list(i), b_list(i), "=> ", g_list(i)
    end do

    contains

    recursive function gcd(a, b) result(g)
        integer, value :: a, b
        integer :: t
        integer :: g
        
        if (b == 0) then
            g = a
            return
        end if

        t = b
        b = mod(a, b)
        a = t
        g = gcd(a, b)

    end function gcd

end program commondivisor