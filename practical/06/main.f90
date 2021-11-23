program matrix
    implicit none
    integer, parameter :: n = 5, p = 3, q = 4
    integer :: a(n, p), b(p, q), c(n, q)
    integer :: i, j

    ! initialize three arrays
    ! I should be able to do that with implicit loop or something
    do, i = 1, n
        do, j = 1, p
            a(i, j) = i + j
        end do
    end do
    do, i = 1, p
        do, j = 1, q
            b(i, j) = i - j
        end do
    end do
    c = 0

    ! call matmult(a, b, c)
    
    call print_matrix(a)
    call print_matrix(b)
    call print_matrix(c)
    
    contains

    subroutine print_matrix(m)
        integer :: i, j
        integer, dimension(:,:), intent(in) :: m
        do, i = 1, size(m, 1)
            write(*,"(999I4)") (m(i, j), j=1, size(m, 2))
        end do
        write(*, *)
    end subroutine

end program matrix