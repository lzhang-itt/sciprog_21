subroutine matmult(x, y, z)
    integer, dimension(:, :), intent(in) :: x, y
    integer, dimension(:, :), intent(inout) :: z
    integer :: i, j

    do, i = 1, size(z, 1)
        do, j = 1, size(z, 2)
            z(i, j) = sum(x(i, :)*y(:, j))
        end do
    end do 
    
end subroutine