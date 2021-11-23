subroutine matmult(x, y, z)
    real(8), dimension(:, :), intent(in) :: x, y
    real(8), dimension(:, :), intent(inout) :: z
    integer :: i, j

    do, i = 1, size(z, 1)
        do, j = 1, size(z, 2)
            z(i, j) = sum(x(i, :)*y(:, j))
        end do
    end do 
    
end subroutine